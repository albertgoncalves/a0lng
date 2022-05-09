#include "prelude.h"

#define CAP_TOKENS (1 << 7)
#define CAP_NODES  (1 << 7)
#define CAP_VARS   (1 << 6)
#define CAP_SCOPES (1 << 6)

#ifdef DEBUG
    #define TRACE(expr)                                  \
        {                                                \
            printf("\n%s:%d\n    ", __func__, __LINE__); \
            print_expr(expr);                            \
            printf("\n");                                \
        }
#else
    #define TRACE(_) \
        {}
#endif

typedef union {
    String as_string;
    i64    as_i64;
} TokenBody;

typedef enum {
    TOKEN_END = 0,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_BACKSLASH,
    TOKEN_ARROW,
    TOKEN_SEMICOLON,
    TOKEN_ASSIGN,
    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_IDENT,
    TOKEN_I64,
    TOKEN_EMPTY,
    TOKEN_IF,
} TokenTag;

typedef struct {
    TokenBody body;
    TokenTag  tag;
} Token;

typedef struct AstExpr AstExpr;

typedef struct {
    String         label;
    const AstExpr* expr;
} AstFn1;

typedef enum {
    INTRIN_SEMICOLON,
    INTRIN_ASSIGN,
    INTRIN_ADD,
    INTRIN_SUB,
    INTRIN_MUL,
    INTRIN_DIV,
} AstIntrinTag;

typedef struct {
    const AstExpr* expr;
    AstIntrinTag   tag;
} AstIntrin;

typedef struct {
    const AstExpr* func;
    const AstExpr* arg;
} AstCall;

typedef struct {
    const AstExpr* cond;
    const AstExpr* if_true;
} AstIf;

typedef union {
    AstCall        as_call;
    const AstExpr* as_fn0;
    AstFn1         as_fn1;
    String         as_string;
    i64            as_i64;
    AstIntrin      as_intrinsic;
    AstIf          as_if;
} AstExprBody;

typedef enum {
    AST_EXPR_EMPTY = 0,
    AST_EXPR_CALL,
    AST_EXPR_IDENT,
    AST_EXPR_I64,
    AST_EXPR_FN0,
    AST_EXPR_FN1,
    AST_EXPR_INTRIN,
    AST_EXPR_IF,
} AstExprTag;

struct AstExpr {
    AstExprBody body;
    AstExprTag  tag;
};

typedef struct Var   Var;
typedef struct Scope Scope;

typedef struct {
    Scope*         scope;
    const AstExpr* expr;
} Env;

struct Var {
    String label;
    Env    env;
    Var*   next;
};

struct Scope {
    Var*   vars;
    Scope* next;
};

typedef struct {
    Token   tokens[CAP_TOKENS];
    u32     len_tokens;
    AstExpr nodes[CAP_NODES];
    u32     len_nodes;
    Var     vars[CAP_VARS];
    u32     len_vars;
    Scope   scopes[CAP_SCOPES];
    u32     len_scopes;
} Memory;

static const AstExpr I64_ZERO = (AstExpr){
    .tag  = AST_EXPR_I64,
    .body = {.as_i64 = 0},
};

#define IS_DIGIT(x) (('0' <= (x)) && ((x) <= '9'))

#define IS_ALPHA(x) \
    ((('A' <= (x)) && ((x) <= 'Z')) || (('a' <= (x)) && ((x) <= 'z')))

#define IS_IDENT(x) (IS_ALPHA(x) || IS_DIGIT(x) || (x == '_'))

static Memory* alloc_memory(void) {
    void* address = mmap(NULL,
                         sizeof(Memory),
                         PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE,
                         -1,
                         0);
    EXIT_IF(address == MAP_FAILED);
    Memory* memory     = (Memory*)address;
    memory->len_nodes  = 0;
    memory->len_vars   = 0;
    memory->len_scopes = 0;
    return memory;
}

static Token* alloc_token(Memory* memory) {
    EXIT_IF(CAP_TOKENS <= memory->len_tokens);
    return &memory->tokens[memory->len_tokens++];
}

static void tokenize(Memory* memory, String string) {
    for (u32 i = 0; i < string.len;) {
        switch (string.buffer[i]) {
        case ' ':
        case '\t':
        case '\n': {
            ++i;
            break;
        }
        case '#': {
            while ((i < string.len) && (string.buffer[i++] != '\n')) {
            }
            break;
        }
        case '(': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_LPAREN;
            ++i;
            break;
        }
        case ')': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_RPAREN;
            ++i;
            break;
        }
        case '\\': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_BACKSLASH;
            ++i;
            break;
        }
        case ';': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_SEMICOLON;
            ++i;
            break;
        }
        case '_': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_EMPTY;
            ++i;
            break;
        }
        case '+': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_ADD;
            ++i;
            break;
        }
        case '*': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_MUL;
            ++i;
            break;
        }
        case '=': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_ASSIGN;
            ++i;
            break;
        }
        case '-': {
            Token* token = alloc_token(memory);
            token->tag   = TOKEN_SUB;
            ++i;
            if (!(i < string.len)) {
                continue;
            }
            switch (string.buffer[i]) {
            case '>': {
                token->tag = TOKEN_ARROW;
                ++i;
                break;
            }
            default: {
            }
            }
            break;
        }
        default: {
            if (IS_DIGIT(string.buffer[i])) {
                i64 x = 0;
                for (; i < string.len; ++i) {
                    if (!IS_DIGIT(string.buffer[i])) {
                        break;
                    }
                    x = (x * 10) + ((i64)(string.buffer[i] - '0'));
                }
                Token* token       = alloc_token(memory);
                token->tag         = TOKEN_I64;
                token->body.as_i64 = x;
                continue;
            }
            if (IS_ALPHA(string.buffer[i])) {
                u32 j = i;
                for (; i < string.len; ++i) {
                    if (!IS_IDENT(string.buffer[i])) {
                        break;
                    }
                }
                String ident = {.buffer = &string.buffer[j], .len = i - j};
                Token* token = alloc_token(memory);
                if (eq(ident, STRING("if"))) {
                    token->tag = TOKEN_IF;
                    continue;
                }
                token->tag            = TOKEN_IDENT;
                token->body.as_string = ident;
                continue;
            }
            EXIT();
        }
        }
    }
}

static AstExpr* alloc_expr(Memory* memory) {
    EXIT_IF(CAP_NODES <= memory->len_nodes);
    return &memory->nodes[memory->len_nodes++];
}

static const AstExpr* alloc_expr_ident(Memory* memory, String string) {
    AstExpr* expr        = alloc_expr(memory);
    expr->tag            = AST_EXPR_IDENT;
    expr->body.as_string = string;
    return expr;
}

static const AstExpr* alloc_expr_i64(Memory* memory, i64 x) {
    AstExpr* expr     = alloc_expr(memory);
    expr->tag         = AST_EXPR_I64;
    expr->body.as_i64 = x;
    return expr;
}

static const AstExpr* alloc_expr_empty(Memory* memory) {
    AstExpr* expr = alloc_expr(memory);
    expr->tag     = AST_EXPR_EMPTY;
    return expr;
}

static const AstExpr* alloc_expr_call(Memory*        memory,
                                      const AstExpr* func,
                                      const AstExpr* arg) {
    AstExpr* expr           = alloc_expr(memory);
    expr->tag               = AST_EXPR_CALL;
    expr->body.as_call.func = func;
    expr->body.as_call.arg  = arg;
    return expr;
}

static const AstExpr* alloc_expr_intrinsic(Memory*        memory,
                                           AstIntrinTag   tag,
                                           const AstExpr* expr) {
    AstExpr* intrinsic                = alloc_expr(memory);
    intrinsic->tag                    = AST_EXPR_INTRIN;
    intrinsic->body.as_intrinsic.tag  = tag;
    intrinsic->body.as_intrinsic.expr = expr;
    return intrinsic;
}

static const AstExpr* alloc_expr_if(Memory*        memory,
                                    const AstExpr* condition,
                                    const AstExpr* if_true) {
    AstExpr* expr            = alloc_expr(memory);
    expr->tag                = AST_EXPR_IF;
    expr->body.as_if.cond    = condition;
    expr->body.as_if.if_true = if_true;
    return expr;
}

static Var* alloc_var(Memory* memory) {
    EXIT_IF(CAP_VARS <= memory->len_vars);
    Var* var   = &memory->vars[memory->len_vars++];
    var->label = (String){0};
    var->env   = (Env){0};
    var->next  = NULL;
    return var;
}

static Scope* alloc_scope(Memory* memory) {
    EXIT_IF(CAP_SCOPES <= memory->len_scopes);
    Scope* scope = &memory->scopes[memory->len_scopes++];
    scope->vars  = NULL;
    scope->next  = NULL;
    return scope;
}

static Var* lookup_var(Var* var, String label) {
    while (var) {
        if (eq(label, var->label)) {
            return var;
        }
        var = var->next;
    }
    return NULL;
}

static Var* lookup_scope(const Scope* scope, String label) {
    while (scope) {
        Var* var = lookup_var(scope->vars, label);
        if (var) {
            return var;
        }
        scope = scope->next;
    }
    return NULL;
}

static void push_var(Memory* memory, Scope* scope, String label, Env env) {
    Var* var    = alloc_var(memory);
    var->label  = label;
    var->env    = env;
    var->next   = scope->vars;
    scope->vars = var;
}

static Scope* push_scope(Memory* memory, Scope* parent) {
    Scope* child = alloc_scope(memory);
    child->next  = parent;
    return child;
}

static void print_token(Token token) {
    switch (token.tag) {
    case TOKEN_IDENT: {
        print_string(token.body.as_string);
        break;
    }
    case TOKEN_I64: {
        printf("%ld", token.body.as_i64);
        break;
    }
    case TOKEN_LPAREN: {
        putchar('(');
        break;
    }
    case TOKEN_RPAREN: {
        putchar(')');
        break;
    }
    case TOKEN_BACKSLASH: {
        putchar('\\');
        break;
    }
    case TOKEN_ARROW: {
        printf("->");
        break;
    }
    case TOKEN_SEMICOLON: {
        putchar(';');
        break;
    }
    case TOKEN_ASSIGN: {
        putchar('=');
        break;
    }
    case TOKEN_ADD: {
        putchar('+');
        break;
    }
    case TOKEN_SUB: {
        putchar('-');
        break;
    }
    case TOKEN_MUL: {
        putchar('*');
        break;
    }
    case TOKEN_DIV: {
        putchar('/');
        break;
    }
    case TOKEN_EMPTY: {
        putchar('_');
        break;
    }
    case TOKEN_IF: {
        printf("if");
        break;
    }
    case TOKEN_END:
    default: {
        EXIT();
    }
    }
}

const AstExpr* parse_expr(Memory*, const Token**, u32, u32);

static const AstExpr* parse_fn(Memory*       memory,
                               const Token** tokens,
                               u32           depth) {
    if ((*tokens)->tag != TOKEN_IDENT) {
        EXIT_IF((*tokens)->tag != TOKEN_ARROW);
        AstExpr* expr = alloc_expr(memory);
        expr->tag     = AST_EXPR_FN0;
        ++(*tokens);
        expr->body.as_fn0 = parse_expr(memory, tokens, 0, depth);
        return expr;
    }
    EXIT_IF((*tokens)->tag != TOKEN_IDENT);
    AstExpr* expr           = alloc_expr(memory);
    expr->tag               = AST_EXPR_FN1;
    expr->body.as_fn1.label = (*tokens)->body.as_string;
    ++(*tokens);
    EXIT_IF((*tokens)->tag != TOKEN_ARROW);
    ++(*tokens);
    expr->body.as_fn1.expr = parse_expr(memory, tokens, 0, depth);
    return expr;
}

#define PARSE_INFIX(tag, binding_left, binding_right)          \
    {                                                          \
        if (binding_left < binding) {                          \
            return expr;                                       \
        }                                                      \
        ++(*tokens);                                           \
        expr = alloc_expr_call(                                \
            memory,                                            \
            alloc_expr_intrinsic(memory, tag, expr),           \
            parse_expr(memory, tokens, binding_right, depth)); \
    }

static const AstExpr* parse_if(Memory* memory, const Token** tokens) {
    EXIT_IF((*tokens)->tag != TOKEN_LPAREN);
    ++(*tokens);
    const AstExpr* condition = parse_expr(memory, tokens, 0, 1);
    EXIT_IF((*tokens)->tag != TOKEN_RPAREN);
    ++(*tokens);
    EXIT_IF((*tokens)->tag != TOKEN_LPAREN);
    ++(*tokens);
    const AstExpr* if_true = parse_expr(memory, tokens, 0, 1);
    EXIT_IF((*tokens)->tag != TOKEN_RPAREN);
    ++(*tokens);
    return alloc_expr_if(memory, condition, if_true);
}

const AstExpr* parse_expr(Memory*       memory,
                          const Token** tokens,
                          u32           binding,
                          u32           depth) {
    const AstExpr* expr;
    switch ((*tokens)->tag) {
    case TOKEN_IF: {
        ++(*tokens);
        expr = parse_if(memory, tokens);
        break;
    }
    case TOKEN_LPAREN: {
        ++(*tokens);
        expr = parse_expr(memory, tokens, 0, depth + 1);
        EXIT_IF((*tokens)->tag != TOKEN_RPAREN);
        ++(*tokens);
        break;
    }
    case TOKEN_IDENT: {
        expr = alloc_expr_ident(memory, (*tokens)->body.as_string);
        ++(*tokens);
        break;
    }
    case TOKEN_I64: {
        expr = alloc_expr_i64(memory, (*tokens)->body.as_i64);
        ++(*tokens);
        break;
    }
    case TOKEN_BACKSLASH: {
        ++(*tokens);
        expr = parse_fn(memory, tokens, depth);
        break;
    }
    case TOKEN_EMPTY: {
        expr = alloc_expr_empty(memory);
        ++(*tokens);
        break;
    }
    case TOKEN_SUB: {
#define BINDING_RIGHT 9
        ++(*tokens);
        expr = alloc_expr_call(
            memory,
            alloc_expr_intrinsic(memory, INTRIN_SUB, &I64_ZERO),
            parse_expr(memory, tokens, BINDING_RIGHT, depth));
        break;
#undef BINDING_RIGHT
    }
    case TOKEN_RPAREN:
    case TOKEN_ARROW:
    case TOKEN_SEMICOLON:
    case TOKEN_ASSIGN:
    case TOKEN_ADD:
    case TOKEN_MUL:
    case TOKEN_DIV:
    case TOKEN_END:
    default: {
        print_token(**tokens);
        putchar('\n');
        EXIT();
    }
    }
    for (;;) {
        switch ((*tokens)->tag) {
        case TOKEN_END: {
            return expr;
        }
        case TOKEN_IDENT:
        case TOKEN_I64:
        case TOKEN_EMPTY: {
#define BINDING_LEFT  11
#define BINDING_RIGHT 12
            if (BINDING_LEFT < binding) {
                return expr;
            }
            expr = alloc_expr_call(
                memory,
                expr,
                parse_expr(memory, tokens, BINDING_RIGHT, depth));
            break;
        }
        case TOKEN_LPAREN: {
            ++(*tokens);
            if (BINDING_LEFT < binding) {
                return expr;
            }
            expr = alloc_expr_call(memory,
                                   expr,
                                   parse_expr(memory, tokens, 0, depth + 1));
            EXIT_IF((*tokens)->tag != TOKEN_RPAREN);
            ++(*tokens);
            break;
        }
        case TOKEN_BACKSLASH: {
            if (BINDING_LEFT < binding) {
                return expr;
            }
            ++(*tokens);
            expr =
                alloc_expr_call(memory, expr, parse_fn(memory, tokens, depth));
            break;
#undef BINDING_LEFT
#undef BINDING_RIGHT
        }
        case TOKEN_ADD: {
            PARSE_INFIX(INTRIN_ADD, 5, 6);
            break;
        }
        case TOKEN_SUB: {
            PARSE_INFIX(INTRIN_SUB, 5, 6);
            break;
        }
        case TOKEN_MUL: {
            PARSE_INFIX(INTRIN_MUL, 7, 8);
            break;
        }
        case TOKEN_DIV: {
            PARSE_INFIX(INTRIN_DIV, 7, 8);
            break;
        }
        case TOKEN_ASSIGN: {
            PARSE_INFIX(INTRIN_ASSIGN, 4, 3);
            break;
        }
        case TOKEN_SEMICOLON: {
            PARSE_INFIX(INTRIN_SEMICOLON, 1, 2);
            break;
        }
        case TOKEN_RPAREN: {
            EXIT_IF(depth == 0);
            return expr;
        }
        case TOKEN_IF:
        case TOKEN_ARROW:
        default: {
            print_token(**tokens);
            putchar('\n');
            EXIT();
        }
        }
    }
}

#undef PARSE_INFIX

static void print_intrinsic(AstIntrinTag tag) {
    switch (tag) {
    case INTRIN_SEMICOLON: {
        putchar(';');
        break;
    }
    case INTRIN_ASSIGN: {
        putchar('=');
        break;
    }
    case INTRIN_ADD: {
        putchar('+');
        break;
    }
    case INTRIN_SUB: {
        putchar('-');
        break;
    }
    case INTRIN_MUL: {
        putchar('*');
        break;
    }
    case INTRIN_DIV: {
        putchar('/');
        break;
    }
    default: {
        EXIT();
    }
    }
}

static void print_expr(const AstExpr* expr) {
    switch (expr->tag) {
    case AST_EXPR_IDENT: {
        print_string(expr->body.as_string);
        break;
    }
    case AST_EXPR_I64: {
        printf("%ld", expr->body.as_i64);
        break;
    }
    case AST_EXPR_CALL: {
        print_expr(expr->body.as_call.func);
        putchar('(');
        print_expr(expr->body.as_call.arg);
        putchar(')');
        break;
    }
    case AST_EXPR_FN0: {
        printf("(\\");
        printf(" -> ");
        print_expr(expr->body.as_fn0);
        putchar(')');
        break;
    }
    case AST_EXPR_FN1: {
        printf("(\\");
        print_string(expr->body.as_fn1.label);
        printf(" -> ");
        print_expr(expr->body.as_fn1.expr);
        putchar(')');
        break;
    }
    case AST_EXPR_INTRIN: {
        putchar('(');
        print_expr(expr->body.as_intrinsic.expr);
        printf(") ");
        print_intrinsic(expr->body.as_intrinsic.tag);
        putchar(' ');
        break;
    }
    case AST_EXPR_EMPTY: {
        break;
    }
    case AST_EXPR_IF: {
        printf("if(");
        print_expr(expr->body.as_if.cond);
        printf(")(");
        print_expr(expr->body.as_call.arg);
        putchar(')');
        break;
    }
    default: {
        EXIT();
    }
    }
}

Env eval_expr(Memory*, Env);

#define BINOP_I64(op)                                                       \
    {                                                                       \
        Env l = {                                                           \
            .scope = scope,                                                 \
            .expr  = intrinsic.expr,                                        \
        };                                                                  \
        l = eval_expr(memory, l);                                           \
        EXIT_IF(!l.expr);                                                   \
        EXIT_IF(l.expr->tag != AST_EXPR_I64);                               \
        Env r = {                                                           \
            .scope = scope,                                                 \
            .expr  = arg,                                                   \
        };                                                                  \
        r = eval_expr(memory, r);                                           \
        EXIT_IF(!r.expr);                                                   \
        EXIT_IF(r.expr->tag != AST_EXPR_I64);                               \
        return (Env){                                                       \
            .scope = scope,                                                 \
            .expr =                                                         \
                alloc_expr_i64(memory,                                      \
                               l.expr->body.as_i64 op r.expr->body.as_i64), \
        };                                                                  \
    }

static Env eval_expr_intrinsic(Memory*        memory,
                               Scope*         scope,
                               AstIntrin      intrinsic,
                               const AstExpr* arg) {
    TRACE(intrinsic.expr);
    EXIT_IF(!intrinsic.expr);
    switch (intrinsic.tag) {
    case INTRIN_SEMICOLON: {
        Env env = {
            .scope = scope,
            .expr  = intrinsic.expr,
        };
        env      = eval_expr(memory, env);
        env.expr = arg;
        return eval_expr(memory, (Env){.scope = scope, .expr = arg});
    }
    case INTRIN_ASSIGN: {
        EXIT_IF(intrinsic.expr->tag != AST_EXPR_IDENT);
        Env env = eval_expr(memory, (Env){.scope = scope, .expr = arg});
        EXIT_IF(!env.expr);
        Var* var = lookup_scope(scope, intrinsic.expr->body.as_string);
        if (var) {
            var->env = env;
        } else {
            push_var(memory, scope, intrinsic.expr->body.as_string, env);
        }
        return (Env){
            .scope = scope,
            .expr  = NULL,
        };
    }
    case INTRIN_ADD: {
        BINOP_I64(+);
    }
    case INTRIN_SUB: {
        BINOP_I64(-);
    }
    case INTRIN_MUL: {
        BINOP_I64(*);
    }
    case INTRIN_DIV: {
        BINOP_I64(/);
    }
    default: {
        EXIT();
    }
    }
}

#undef BINOP_I64

static Env eval_expr_call(Memory*        memory,
                          Scope*         scope,
                          const AstExpr* func,
                          const AstExpr* arg) {
    TRACE(func);
    EXIT_IF(!func);
    EXIT_IF(!arg);
    switch (func->tag) {
    case AST_EXPR_INTRIN: {
        return eval_expr_intrinsic(memory,
                                   scope,
                                   func->body.as_intrinsic,
                                   arg);
    }
    case AST_EXPR_IDENT: {
        Env env = eval_expr(memory, (Env){.scope = scope, .expr = func});
        EXIT_IF(!env.expr);
        return eval_expr_call(memory, env.scope, env.expr, arg);
    }
    case AST_EXPR_CALL: {
        Env env = eval_expr_call(memory,
                                 scope,
                                 func->body.as_call.func,
                                 func->body.as_call.arg);
        EXIT_IF(!env.expr);
        return eval_expr_call(memory, env.scope, env.expr, arg);
    }
    case AST_EXPR_FN0: {
        scope = push_scope(memory, scope);
        return eval_expr(memory,
                         (Env){.scope = scope, .expr = func->body.as_fn0});
    }
    case AST_EXPR_FN1: {
        scope = push_scope(memory, scope);
        push_var(memory,
                 scope,
                 func->body.as_fn1.label,
                 (Env){.scope = scope, .expr = arg});
        return eval_expr(
            memory,
            (Env){.scope = scope, .expr = func->body.as_fn1.expr});
    }
    case AST_EXPR_IF:
    case AST_EXPR_I64:
    case AST_EXPR_EMPTY:
    default: {
        EXIT();
    }
    }
}

Env eval_expr(Memory* memory, Env env) {
    TRACE(env.expr);
    EXIT_IF(!env.expr);
    switch (env.expr->tag) {
    case AST_EXPR_IDENT: {
        Var* var = lookup_scope(env.scope, env.expr->body.as_string);
        EXIT_IF(!var);
        return var->env;
    }
    case AST_EXPR_I64:
    case AST_EXPR_FN0:
    case AST_EXPR_FN1:
    case AST_EXPR_INTRIN: {
        return env;
    }
    case AST_EXPR_CALL: {
        return eval_expr_call(memory,
                              env.scope,
                              env.expr->body.as_call.func,
                              env.expr->body.as_call.arg);
    }
    case AST_EXPR_IF: {
        const AstExpr* condition =
            eval_expr(
                memory,
                (Env){.scope = env.scope, .expr = env.expr->body.as_if.cond})
                .expr;
        EXIT_IF(condition->tag != AST_EXPR_I64);
        if (condition->body.as_i64) {
            eval_expr(memory,
                      (Env){.scope = env.scope,
                            .expr  = env.expr->body.as_if.if_true});
        }
        env.expr = NULL;
        return env;
    }
    case AST_EXPR_EMPTY:
    default: {
        EXIT();
    }
    }
}

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
#ifdef DEBUG
    printf("\n"
           "sizeof(Token)       : %zu\n"
           "sizeof(AstIntrin)   : %zu\n"
           "sizeof(AstExpr)     : %zu\n"
           "sizeof(Env)         : %zu\n"
           "sizeof(Var)         : %zu\n"
           "sizeof(Scope)       : %zu\n"
           "sizeof(Memory)      : %zu\n"
           "\n",
           sizeof(Token),
           sizeof(AstIntrin),
           sizeof(AstExpr),
           sizeof(Env),
           sizeof(Var),
           sizeof(Scope),
           sizeof(Memory));
#endif
    Memory* memory = alloc_memory();
    tokenize(memory, path_to_string(args[1]));
    const Token*   tokens = memory->tokens;
    const AstExpr* expr   = parse_expr(memory, &tokens, 0, 0);
    print_expr(
        eval_expr(memory, (Env){.scope = alloc_scope(memory), .expr = expr})
            .expr);
    putchar('\n');
    return OK;
}
