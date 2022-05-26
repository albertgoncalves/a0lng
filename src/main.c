#include "prelude.h"

#define CAP_BUFFER (1 << 6)
#define CAP_TOKENS (1 << 8)
#define CAP_EXPRS  (1 << 9)
#define CAP_VARS   (1 << 10)
#define CAP_SCOPES (1 << 10)

typedef union {
    String as_string;
    i64    as_i64;
} TokenBody;

typedef enum {
    TOKEN_ERROR = 0,
    TOKEN_END,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_DOT,
    TOKEN_BACKSLASH,
    TOKEN_ARROW,
    TOKEN_SEMICOLON,
    TOKEN_ASSIGN,
    TOKEN_UPDATE,
    TOKEN_EQ,
    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_IDENT,
    TOKEN_I64,
    TOKEN_STRING,
    TOKEN_IF,
    TOKEN_THEN,
    TOKEN_ELSE,
    TOKEN_RETURN,
    TOKEN_COUNT,
} TokenTag;

typedef struct {
    TokenBody body;
    TokenTag  tag;
    u32       offset;
} Token;

typedef struct Expr Expr;

typedef struct {
    Expr* expr;
} ExprList;

typedef struct {
    ExprList list;
} Fn0;

typedef struct {
    String   label;
    ExprList list;
} Fn1;

typedef enum {
    INTRIN_ERROR = 0,
    INTRIN_ASSIGN,
    INTRIN_UPDATE,
    INTRIN_ACCESS,
    INTRIN_EQ,
    INTRIN_ADD,
    INTRIN_SUB,
    INTRIN_MUL,
    INTRIN_DIV,
    INTRIN_PRINT,
    INTRIN_TOSTRING,
} IntrinTag;

typedef struct {
    Expr*     expr;
    IntrinTag tag;
} Intrin;

typedef struct {
    Expr* func;
    Expr* arg;
} Call;

typedef struct {
    Expr* condition;
    Expr* if_then;
    Expr* if_else;
} If;

typedef struct Var Var;

typedef union {
    Call   as_call;
    Fn0    as_fn0;
    Fn1    as_fn1;
    String as_string;
    i64    as_i64;
    Intrin as_intrinsic;
    If     as_if;
    Var*   as_object;
} ExprBody;

typedef enum {
    EXPR_ERROR = 0,
    EXPR_EMPTY,
    EXPR_CALL,
    EXPR_IDENT,
    EXPR_I64,
    EXPR_FN0,
    EXPR_FN1,
    EXPR_INTRIN,
    EXPR_IF_ELSE,
    EXPR_OBJECT,
    EXPR_STRING,
} ExprTag;

struct Expr {
    ExprList next;
    ExprBody body;
    ExprTag  tag;
    u32      offset;
};

typedef struct Scope Scope;

typedef struct {
    Scope* scope;
    Expr*  expr;
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
    const char* source_path;
    String      source_string;
    u32         len_buffer;
    u32         len_tokens;
    u32         len_exprs;
    u32         len_vars;
    u32         len_scopes;
    char        buffer[CAP_BUFFER];
    Token       tokens[CAP_TOKENS];
    Expr        exprs[CAP_EXPRS];
    Var         vars[CAP_VARS];
    Scope       scopes[CAP_SCOPES];
} Memory;

static Expr EMPTY = (Expr){
    .tag = EXPR_EMPTY,
    .offset = 0,
};

static Expr I64_ZERO = (Expr){
    .tag = EXPR_I64,
    .body = {.as_i64 = 0},
    .offset = 0,
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
    Memory* memory = (Memory*)address;
    memset(memory, 0, sizeof(Memory));
    return memory;
}

static void print_row_col(Memory* memory, u32 offset) {
    u32 row = 1;
    u32 col = 1;
    for (u32 i = 0; i < offset; ++i) {
        EXIT_IF(memory->source_string.len <= i);
        if (memory->source_string.buffer[i] == '\n') {
            ++row;
            col = 1;
        } else {
            ++col;
        }
    }
    printf("%s:%u:%u", memory->source_path, row, col);
}

#define EXIT_LOC(memory, offset)                            \
    {                                                       \
        printf("%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        print_row_col(memory, offset);                      \
        putchar('\n');                                      \
        _exit(ERROR);                                       \
    }

#define EXIT_IF_LOC(condition, memory, offset)                               \
    if (condition) {                                                         \
        printf("%s:%s:%d `%s`\n", __FILE__, __func__, __LINE__, #condition); \
        print_row_col(memory, offset);                                       \
        putchar('\n');                                                       \
        _exit(ERROR);                                                        \
    }

static void push_buffer(Memory* memory, char x) {
    EXIT_IF(CAP_BUFFER <= memory->len_buffer);
    memory->buffer[memory->len_buffer++] = x;
}

static String i64_to_string(Memory* memory, i64 x) {
    u32    len = i64_to_len(x);
    char*  buffer = &memory->buffer[memory->len_buffer];
    String string = {
        .buffer = buffer,
        .len = len,
    };
    if (x < 0) {
        push_buffer(memory, '-');
        ++string.len;
        ++buffer;
        x = -x;
    }
    EXIT_IF(CAP_BUFFER < (memory->len_buffer + len));
    for (u32 i = 0; i < len; ++i) {
        buffer[(len - i) - 1] = '0' + ((char)(x % 10));
        x /= 10;
    }
    memory->len_buffer += string.len;
    return string;
}

static Token* alloc_token(Memory* memory, TokenTag tag, u32 offset) {
    EXIT_IF(CAP_TOKENS <= memory->len_tokens);
    Token* token = &memory->tokens[memory->len_tokens++];
    token->tag = tag;
    token->offset = offset;
    return token;
}

STATIC_ASSERT(TOKEN_COUNT == 24);

static void tokenize(Memory* memory) {
    u32 i;
    for (i = 0; i < memory->source_string.len;) {
        switch (memory->source_string.buffer[i]) {
        case ' ':
        case '\t':
        case '\n': {
            ++i;
            break;
        }
        case '#': {
            while ((i < memory->source_string.len) &&
                   (memory->source_string.buffer[i++] != '\n'))
            {}
            break;
        }
        case '.': {
            alloc_token(memory, TOKEN_DOT, i++);
            break;
        }
        case '(': {
            alloc_token(memory, TOKEN_LPAREN, i++);
            break;
        }
        case ')': {
            alloc_token(memory, TOKEN_RPAREN, i++);
            break;
        }
        case '{': {
            alloc_token(memory, TOKEN_LBRACE, i++);
            break;
        }
        case '}': {
            alloc_token(memory, TOKEN_RBRACE, i++);
            break;
        }
        case '\\': {
            alloc_token(memory, TOKEN_BACKSLASH, i++);
            break;
        }
        case ';': {
            alloc_token(memory, TOKEN_SEMICOLON, i++);
            break;
        }
        case '+': {
            alloc_token(memory, TOKEN_ADD, i++);
            break;
        }
        case '*': {
            alloc_token(memory, TOKEN_MUL, i++);
            break;
        }
        case '/': {
            alloc_token(memory, TOKEN_DIV, i++);
            break;
        }
        case ':': {
            EXIT_IF_LOC(!(i < memory->source_string.len),
                        memory,
                        memory->source_string.len);
            ++i;
            switch (memory->source_string.buffer[i]) {
            case '=': {
                alloc_token(memory, TOKEN_ASSIGN, i++ - 1);
                break;
            }
            default: {
                EXIT_LOC(memory, i);
            }
            }
            break;
        }
        case '=': {
            Token* token = alloc_token(memory, TOKEN_UPDATE, i++);
            if (!(i < memory->source_string.len)) {
                continue;
            }
            switch (memory->source_string.buffer[i]) {
            case '=': {
                token->tag = TOKEN_EQ;
                ++i;
                break;
            }
            default: {
            }
            }
            break;
        }
        case '-': {
            Token* token = alloc_token(memory, TOKEN_SUB, i++);
            if (!(i < memory->source_string.len)) {
                continue;
            }
            switch (memory->source_string.buffer[i]) {
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
        case '"': {
            Token* token = alloc_token(memory, TOKEN_STRING, i);
            token->body.as_string = (String){
                .buffer = &memory->buffer[memory->len_buffer],
                .len = 0,
            };
            ++i;
            EXIT_IF_LOC(memory->source_string.len <= i,
                        memory,
                        memory->source_string.len);
            while (memory->source_string.buffer[i] != '"') {
                if (memory->source_string.buffer[i] == '\\') {
                    EXIT_IF_LOC(memory->source_string.len <= (i + 1),
                                memory,
                                memory->source_string.len);
                    switch (memory->source_string.buffer[i + 1]) {
                    case 'n': {
                        push_buffer(memory, '\n');
                        ++token->body.as_string.len;
                        i += 2;
                        continue;
                    }
                    case '"': {
                        push_buffer(memory, '"');
                        ++token->body.as_string.len;
                        i += 2;
                        continue;
                    }
                    case '\\': {
                        push_buffer(memory, '\\');
                        ++token->body.as_string.len;
                        i += 2;
                        continue;
                    }
                    }
                }
                push_buffer(memory, memory->source_string.buffer[i]);
                ++token->body.as_string.len;
                ++i;
                EXIT_IF_LOC(memory->source_string.len <= i,
                            memory,
                            memory->source_string.len);
            }
            ++i;
            break;
        }
        default: {
            if (IS_DIGIT(memory->source_string.buffer[i])) {
                u32 j = i;
                i64 x = 0;
                for (; i < memory->source_string.len; ++i) {
                    if (!IS_DIGIT(memory->source_string.buffer[i])) {
                        break;
                    }
                    x = (x * 10) +
                        ((i64)(memory->source_string.buffer[i] - '0'));
                }
                Token* token = alloc_token(memory, TOKEN_I64, j);
                token->tag = TOKEN_I64;
                token->body.as_i64 = x;
                continue;
            }
            if (IS_ALPHA(memory->source_string.buffer[i])) {
                u32 j = i;
                for (; i < memory->source_string.len; ++i) {
                    if (!IS_IDENT(memory->source_string.buffer[i])) {
                        break;
                    }
                }
                String ident = {
                    .buffer = &memory->source_string.buffer[j],
                    .len = i - j,
                };
                Token* token = alloc_token(memory, TOKEN_IDENT, j);
                if (eq(ident, STRING("if"))) {
                    token->tag = TOKEN_IF;
                    continue;
                }
                if (eq(ident, STRING("then"))) {
                    token->tag = TOKEN_THEN;
                    continue;
                }
                if (eq(ident, STRING("else"))) {
                    token->tag = TOKEN_ELSE;
                    continue;
                }
                if (eq(ident, STRING("return"))) {
                    token->tag = TOKEN_RETURN;
                    continue;
                }
                token->body.as_string = ident;
                continue;
            }
            EXIT_LOC(memory, i);
        }
        }
    }
    alloc_token(memory, TOKEN_END, i);
}

static Expr* alloc_expr(Memory* memory, u32 offset) {
    EXIT_IF(CAP_EXPRS <= memory->len_exprs);
    Expr* expr = &memory->exprs[memory->len_exprs++];
    expr->offset = offset;
    expr->next.expr = NULL;
    return expr;
}

static Expr* alloc_expr_ident(Memory* memory, String ident, u32 offset) {
    Expr* expr = alloc_expr(memory, offset);
    expr->tag = EXPR_IDENT;
    expr->body.as_string = ident;
    return expr;
}

static Expr* alloc_expr_string(Memory* memory, String string, u32 offset) {
    Expr* expr = alloc_expr(memory, offset);
    expr->tag = EXPR_STRING;
    expr->body.as_string = string;
    return expr;
}

static Expr* alloc_expr_i64(Memory* memory, i64 x, u32 offset) {
    Expr* expr = alloc_expr(memory, offset);
    expr->tag = EXPR_I64;
    expr->body.as_i64 = x;
    return expr;
}

static Expr* alloc_expr_empty(Memory* memory, u32 offset) {
    Expr* expr = alloc_expr(memory, offset);
    expr->tag = EXPR_EMPTY;
    return expr;
}

static Expr* alloc_expr_call(Memory* memory, Expr* func, Expr* arg) {
    // NOTE: Is this the right `offset`?
    Expr* expr = alloc_expr(memory, arg->offset);
    expr->tag = EXPR_CALL;
    expr->body.as_call.func = func;
    expr->body.as_call.arg = arg;
    return expr;
}

static Expr* alloc_expr_intrinsic(Memory*   memory,
                                  IntrinTag tag,
                                  Expr*     expr,
                                  u32       offset) {
    Expr* intrinsic = alloc_expr(memory, offset);
    intrinsic->tag = EXPR_INTRIN;
    intrinsic->body.as_intrinsic.tag = tag;
    intrinsic->body.as_intrinsic.expr = expr;
    return intrinsic;
}

static Expr* alloc_expr_fn0(Memory* memory, ExprList list, u32 offset) {
    Expr* expr = alloc_expr(memory, offset);
    expr->tag = EXPR_FN0;
    expr->body.as_fn0.list = list;
    return expr;
}

static Expr* alloc_expr_if_else(Memory* memory,
                                Expr*   condition,
                                Expr*   if_then,
                                Expr*   if_else,
                                u32     offset) {
    Expr* expr = alloc_expr(memory, offset);
    expr->tag = EXPR_IF_ELSE;
    expr->body.as_if.condition = condition;
    expr->body.as_if.if_then = if_then;
    expr->body.as_if.if_else = if_else;
    return expr;
}

static Var* alloc_var(Memory* memory) {
    EXIT_IF(CAP_VARS <= memory->len_vars);
    Var* var = &memory->vars[memory->len_vars++];
    var->label = (String){0};
    var->env = (Env){0};
    var->next = NULL;
    return var;
}

static Scope* alloc_scope(Memory* memory) {
    EXIT_IF(CAP_SCOPES <= memory->len_scopes);
    Scope* scope = &memory->scopes[memory->len_scopes++];
    scope->vars = NULL;
    scope->next = NULL;
    return scope;
}

static Expr* alloc_expr_object(Memory* memory, u32 offset) {
    Expr* expr = alloc_expr(memory, offset);
    expr->tag = EXPR_OBJECT;
    expr->body.as_object = NULL;
    return expr;
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

static void push_var(Memory* memory, Var** vars, String label, Env env) {
    Var* var = alloc_var(memory);
    var->label = label;
    var->env = env;
    var->next = *vars;
    *vars = var;
}

static Scope* push_scope(Memory* memory, Scope* parent) {
    Scope* child = alloc_scope(memory);
    child->next = parent;
    return child;
}

static void push_intrin(Memory*   memory,
                        Scope*    scope,
                        String    string,
                        IntrinTag tag) {
    push_var(memory,
             &scope->vars,
             string,
             (Env){
                 .scope = scope,
                 .expr = alloc_expr_intrinsic(memory, tag, &EMPTY, 0),
             });
}

Expr* parse_expr(Memory*, const Token**, u32, const Token*);
Expr* parse_expr_return_if(Memory*, const Token**, const Token*);

static ExprList parse_exprs(Memory*       memory,
                            const Token** tokens,
                            const Token*  parent) {
    ExprList head = {.expr = (*tokens)->tag == TOKEN_RETURN
                                 ? parse_expr_return_if(memory, tokens, parent)
                                 : parse_expr(memory, tokens, 0, parent)};
    Expr*    tail = head.expr;
    for (;;) {
        EXIT_IF(!tail);
        EXIT_IF_LOC(tail->next.expr, memory, tail->next.expr->offset);
        if ((*tokens)->tag != TOKEN_SEMICOLON) {
            return head;
        }
        ++(*tokens);
        tail->next.expr = (*tokens)->tag == TOKEN_RETURN
                              ? parse_expr_return_if(memory, tokens, parent)
                              : parse_expr(memory, tokens, 0, parent);
        tail = tail->next.expr;
    }
}

static Expr* parse_expr_fn0(Memory*       memory,
                            const Token** tokens,
                            const Token*  parent) {
    EXIT_IF_LOC((*tokens)->tag != TOKEN_ARROW, memory, (*tokens)->offset);
    u32 offset = (*tokens)->offset;
    ++(*tokens);
    return alloc_expr_fn0(memory, parse_exprs(memory, tokens, parent), offset);
}

static Expr* parse_expr_fn1(Memory*       memory,
                            const Token** tokens,
                            const Token*  parent) {
    EXIT_IF_LOC((*tokens)->tag != TOKEN_IDENT, memory, (*tokens)->offset);
    Expr* expr = alloc_expr(memory, (*tokens)->offset);
    expr->tag = EXPR_FN1;
    expr->body.as_fn1.label = (*tokens)->body.as_string;
    ++(*tokens);
    if ((*tokens)->tag == TOKEN_IDENT) {
        expr->body.as_fn1.list = (ExprList){
            .expr = parse_expr_fn1(memory, tokens, parent),
        };
        return expr;
    }
    EXIT_IF_LOC((*tokens)->tag != TOKEN_ARROW, memory, (*tokens)->offset);
    ++(*tokens);
    expr->body.as_fn1.list = parse_exprs(memory, tokens, parent);
    return expr;
}

static Expr* parse_expr_fn(Memory*       memory,
                           const Token** tokens,
                           const Token*  parent) {
    EXIT_IF_LOC((*tokens)->tag != TOKEN_BACKSLASH, memory, (*tokens)->offset);
    ++(*tokens);
    if ((*tokens)->tag != TOKEN_IDENT) {
        return parse_expr_fn0(memory, tokens, parent);
    }
    return parse_expr_fn1(memory, tokens, parent);
}

static Expr* parse_expr_if_else(Memory*       memory,
                                const Token** tokens,
                                const Token*  parent) {
    EXIT_IF_LOC((*tokens)->tag != TOKEN_IF, memory, (*tokens)->offset);
    const Token* parent_if = *tokens;
    ++(*tokens);
    Expr* condition = parse_expr(memory, tokens, 0, parent_if);
    EXIT_IF_LOC((*tokens)->tag != TOKEN_THEN, memory, (*tokens)->offset);
    const Token* parent_then = *tokens;
    ++(*tokens);
    Expr* if_then = parse_expr(memory, tokens, 0, parent_then);
    EXIT_IF_LOC((*tokens)->tag != TOKEN_ELSE, memory, (*tokens)->offset);
    ++(*tokens);
    Expr* if_else = parse_expr(memory, tokens, 0, parent);
    return alloc_expr_if_else(memory,
                              condition,
                              if_then,
                              if_else,
                              parent_if->offset);
}

Expr* parse_expr_return_if(Memory*       memory,
                           const Token** tokens,
                           const Token*  parent) {
    EXIT_IF_LOC((*tokens)->tag != TOKEN_RETURN, memory, (*tokens)->offset);
    const Token* parent_return = *tokens;
    ++(*tokens);
    Expr* return_if = parse_expr(memory, tokens, 0, parent_return);
    EXIT_IF_LOC((*tokens)->tag != TOKEN_IF, memory, (*tokens)->offset);
    const Token* parent_if = *tokens;
    ++(*tokens);
    Expr* condition = parse_expr(memory, tokens, 0, parent_if);
    EXIT_IF_LOC((*tokens)->tag != TOKEN_SEMICOLON, memory, (*tokens)->offset);
    ++(*tokens);
    u32 offset = (*tokens)->offset;
    return alloc_expr_if_else(
        memory,
        condition,
        return_if,
        alloc_expr_call(memory,
                        alloc_expr_fn0(memory,
                                       parse_exprs(memory, tokens, parent),
                                       offset),
                        &EMPTY),
        parent_return->offset);
}

#define PARSE_INFIX(tag, binding_left, binding_right)           \
    {                                                           \
        if (binding_left < binding) {                           \
            return expr;                                        \
        }                                                       \
        u32 offset = (*tokens)->offset;                         \
        ++(*tokens);                                            \
        expr = alloc_expr_call(                                 \
            memory,                                             \
            alloc_expr_intrinsic(memory, tag, expr, offset),    \
            parse_expr(memory, tokens, binding_right, parent)); \
    }

Expr* parse_expr(Memory*       memory,
                 const Token** tokens,
                 u32           binding,
                 const Token*  parent) {
    Expr* expr = NULL;
    switch ((*tokens)->tag) {
    case TOKEN_IF: {
        expr = parse_expr_if_else(memory, tokens, parent);
        break;
    }
    case TOKEN_LPAREN: {
        const Token* parent_paren = *tokens;
        ++(*tokens);
        expr = (*tokens)->tag == TOKEN_RPAREN
                   ? alloc_expr_empty(memory, parent_paren->offset)
                   : parse_expr(memory, tokens, 0, parent_paren);
        EXIT_IF_LOC((*tokens)->tag != TOKEN_RPAREN, memory, (*tokens)->offset);
        ++(*tokens);
        break;
    }
    case TOKEN_IDENT: {
        expr = alloc_expr_ident(memory,
                                (*tokens)->body.as_string,
                                (*tokens)->offset);
        ++(*tokens);
        break;
    }
    case TOKEN_STRING: {
        expr = alloc_expr_string(memory,
                                 (*tokens)->body.as_string,
                                 (*tokens)->offset);
        ++(*tokens);
        break;
    }
    case TOKEN_I64: {
        expr =
            alloc_expr_i64(memory, (*tokens)->body.as_i64, (*tokens)->offset);
        ++(*tokens);
        break;
    }
    case TOKEN_BACKSLASH: {
        expr = parse_expr_fn(memory, tokens, parent);
        break;
    }
    case TOKEN_SUB: {
#define BINDING_RIGHT 11
        u32 offset = (*tokens)->offset;
        ++(*tokens);
        expr = alloc_expr_call(
            memory,
            alloc_expr_intrinsic(memory, INTRIN_SUB, &I64_ZERO, offset),
            parse_expr(memory, tokens, BINDING_RIGHT, parent));
        break;
#undef BINDING_RIGHT
    }
    case TOKEN_LBRACE: {
        u32 offset = (*tokens)->offset;
        ++(*tokens);
        EXIT_IF_LOC((*tokens)->tag != TOKEN_RBRACE, memory, (*tokens)->offset);
        ++(*tokens);
        expr = alloc_expr(memory, offset);
        expr->tag = EXPR_OBJECT;
        break;
    }
    case TOKEN_DOT:
    case TOKEN_RPAREN:
    case TOKEN_RBRACE:
    case TOKEN_ARROW:
    case TOKEN_SEMICOLON:
    case TOKEN_ASSIGN:
    case TOKEN_UPDATE:
    case TOKEN_EQ:
    case TOKEN_ADD:
    case TOKEN_MUL:
    case TOKEN_DIV:
    case TOKEN_THEN:
    case TOKEN_ELSE:
    case TOKEN_RETURN:
    case TOKEN_END:
    case TOKEN_ERROR:
    case TOKEN_COUNT:
    default: {
        EXIT_LOC(memory, (*tokens)->offset);
    }
    }
    for (;;) {
        switch ((*tokens)->tag) {
        case TOKEN_IDENT:
        case TOKEN_I64:
        case TOKEN_LPAREN:
        case TOKEN_STRING:
        case TOKEN_BACKSLASH: {
#define BINDING_LEFT  13
#define BINDING_RIGHT 14
            if (BINDING_LEFT < binding) {
                return expr;
            }
            expr = alloc_expr_call(
                memory,
                expr,
                parse_expr(memory, tokens, BINDING_RIGHT, parent));
            break;
#undef BINDING_LEFT
#undef BINDING_RIGHT
        }
        case TOKEN_ADD: {
            PARSE_INFIX(INTRIN_ADD, 7, 8);
            break;
        }
        case TOKEN_SUB: {
            PARSE_INFIX(INTRIN_SUB, 7, 8);
            break;
        }
        case TOKEN_MUL: {
            PARSE_INFIX(INTRIN_MUL, 9, 10);
            break;
        }
        case TOKEN_DIV: {
            PARSE_INFIX(INTRIN_DIV, 9, 10);
            break;
        }
        case TOKEN_EQ: {
            PARSE_INFIX(INTRIN_EQ, 5, 6);
            break;
        }
        case TOKEN_ASSIGN: {
            PARSE_INFIX(INTRIN_ASSIGN, 4, 3);
            break;
        }
        case TOKEN_UPDATE: {
            PARSE_INFIX(INTRIN_UPDATE, 4, 3);
            break;
        }
        case TOKEN_DOT: {
            PARSE_INFIX(INTRIN_ACCESS, 15, 16);
            break;
        }
#undef PARSE_INFIX
        case TOKEN_RPAREN: {
            EXIT_IF_LOC(!parent, memory, (*tokens)->offset);
            EXIT_IF_LOC(parent->tag != TOKEN_LPAREN,
                        memory,
                        (*tokens)->offset);
            return expr;
        }
        case TOKEN_THEN: {
            EXIT_IF_LOC(!parent, memory, (*tokens)->offset);
            EXIT_IF_LOC(parent->tag != TOKEN_IF, memory, (*tokens)->offset);
            return expr;
        }
        case TOKEN_ELSE: {
            EXIT_IF_LOC(!parent, memory, (*tokens)->offset);
            EXIT_IF_LOC(parent->tag != TOKEN_THEN, memory, (*tokens)->offset);
            return expr;
        }
        case TOKEN_END: {
            EXIT_IF_LOC(parent, memory, (*tokens)->offset);
            return expr;
        }
        case TOKEN_SEMICOLON: {
            return expr;
        }
        case TOKEN_IF: {
            EXIT_IF_LOC(!parent, memory, (*tokens)->offset);
            EXIT_IF_LOC(parent->tag != TOKEN_RETURN,
                        memory,
                        (*tokens)->offset);
            return expr;
        }
        case TOKEN_LBRACE:
        case TOKEN_RBRACE:
        case TOKEN_RETURN:
        case TOKEN_ARROW:
        case TOKEN_ERROR:
        case TOKEN_COUNT:
        default: {
            EXIT_LOC(memory, (*tokens)->offset);
        }
        }
    }
}

static ExprList parse(Memory* memory) {
    const Token*  token_array = memory->tokens;
    const Token** token_pointer = &token_array;
    ExprList      list = parse_exprs(memory, token_pointer, NULL);
    EXIT_IF_LOC((*token_pointer)->tag != TOKEN_END,
                memory,
                (*token_pointer)->offset);
    return list;
}

static void print_intrinsic(IntrinTag tag) {
    switch (tag) {
    case INTRIN_ASSIGN: {
        printf(":=");
        break;
    }
    case INTRIN_UPDATE: {
        putchar('=');
        break;
    }
    case INTRIN_ACCESS: {
        putchar('.');
        break;
    }
    case INTRIN_EQ: {
        printf("==");
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
    case INTRIN_PRINT: {
        printf("print");
        break;
    }
    case INTRIN_TOSTRING: {
        printf("toString");
        break;
    }
    case INTRIN_ERROR: {
        printf("INTRIN_ERROR\n");
        EXIT();
    }
    default: {
        EXIT();
    }
    }
}

void print_exprs(ExprList);

static void print_expr(const Expr* expr) {
    EXIT_IF(!expr);
    switch (expr->tag) {
    case EXPR_IDENT: {
        print_string(expr->body.as_string);
        break;
    }
    case EXPR_STRING: {
        putchar('"');
        print_string(expr->body.as_string);
        putchar('"');
        break;
    }
    case EXPR_I64: {
        printf("%ld", expr->body.as_i64);
        break;
    }
    case EXPR_CALL: {
        putchar('(');
        print_expr(expr->body.as_call.func);
        putchar(' ');
        print_expr(expr->body.as_call.arg);
        putchar(')');
        break;
    }
    case EXPR_FN0: {
        printf("(\\ -> ");
        print_exprs(expr->body.as_fn0.list);
        putchar(')');
        break;
    }
    case EXPR_FN1: {
        printf("(\\");
        print_string(expr->body.as_fn1.label);
        printf(" -> ");
        print_exprs(expr->body.as_fn1.list);
        putchar(')');
        break;
    }
    case EXPR_INTRIN: {
        putchar('(');
        print_expr(expr->body.as_intrinsic.expr);
        putchar(' ');
        print_intrinsic(expr->body.as_intrinsic.tag);
        putchar(')');
        break;
    }
    case EXPR_EMPTY: {
        printf("()");
        break;
    }
    case EXPR_IF_ELSE: {
        printf("(if ");
        print_expr(expr->body.as_if.condition);
        printf(" then ");
        print_expr(expr->body.as_if.if_then);
        printf(" else ");
        print_expr(expr->body.as_if.if_else);
        putchar(')');
        break;
    }
    case EXPR_OBJECT: {
        putchar('{');
        Var* obj = expr->body.as_object;
        if ((!obj) || (!obj->env.expr)) {
            putchar('}');
            break;
        }
        for (;;) {
            putchar('"');
            print_string(obj->label);
            printf("\": ");
            print_expr(obj->env.expr);
            obj = obj->next;
            if ((!obj) || (!obj->env.expr)) {
                break;
            }
            printf(", ");
        }
        putchar('}');
        break;
    }
    case EXPR_ERROR: {
        printf("EXPR_ERROR\n");
        EXIT();
    }
    default: {
        EXIT();
    }
    }
}

void print_exprs(ExprList list) {
    EXIT_IF(!list.expr)
    for (;;) {
        print_expr(list.expr);
        list.expr = list.expr->next.expr;
        if (!list.expr) {
            return;
        }
        printf("; ");
    }
}

Env eval_expr(Memory*, Scope*, Expr*);

static Env eval_exprs(Memory* memory, Scope* scope, ExprList list) {
    EXIT_IF(!scope);
    EXIT_IF(!list.expr);
    Expr* expr = list.expr;
    for (;;) {
        Env env = eval_expr(memory, scope, expr);
        if (!expr->next.expr) {
            return env;
        }
        expr = expr->next.expr;
    }
}

#define BINOP_I64(op)                                                    \
    {                                                                    \
        Expr* l =                                                        \
            eval_expr(memory, scope, func->body.as_intrinsic.expr).expr; \
        EXIT_IF(!l);                                                     \
        EXIT_IF_LOC(l->tag != EXPR_I64, memory, l->offset);              \
        Expr* r = arg.expr;                                              \
        EXIT_IF(!r);                                                     \
        EXIT_IF_LOC(r->tag != EXPR_I64, memory, r->offset);              \
        return (Env){                                                    \
            .scope = scope,                                              \
            .expr = alloc_expr_i64(                                      \
                memory,                                                  \
                l->body.as_i64 op r->body.as_i64,                        \
                func->offset /* NOTE: Is this the right `offset`? */),   \
        };                                                               \
    }

static Env eval_expr_intrinsic(Memory* memory,
                               Scope*  scope,
                               Expr*   func,
                               Env     arg,
                               u32     offset) {
    EXIT_IF(!scope);
    EXIT_IF(!func);
    EXIT_IF(!arg.expr);
    EXIT_IF_LOC(func->tag != EXPR_INTRIN, memory, func->offset);
    EXIT_IF_LOC(func->body.as_intrinsic.expr->next.expr,
                memory,
                func->body.as_intrinsic.expr->next.expr->offset);
    switch (func->body.as_intrinsic.tag) {
    case INTRIN_ASSIGN: {
        func = func->body.as_intrinsic.expr;
        if (func->tag == EXPR_CALL) {
            EXIT_IF_LOC(func->body.as_call.func->tag != EXPR_INTRIN,
                        memory,
                        func->offset);
            EXIT_IF_LOC(func->body.as_call.func->body.as_intrinsic.tag !=
                            INTRIN_ACCESS,
                        memory,
                        func->offset);
            Expr* obj =
                eval_expr(memory,
                          scope,
                          func->body.as_call.func->body.as_intrinsic.expr)
                    .expr;
            EXIT_IF_LOC(obj->tag != EXPR_OBJECT, memory, obj->offset);
            Expr* field =
                eval_expr(memory, scope, func->body.as_call.arg).expr;
            EXIT_IF_LOC(field->tag != EXPR_STRING, memory, field->offset);
            Var* var = lookup_var(obj->body.as_object, field->body.as_string);
            // NOTE: Is this the right `offset`?
            EXIT_IF_LOC(var, memory, func->offset);
            push_var(memory, &obj->body.as_object, field->body.as_string, arg);
        } else if (func->tag == EXPR_IDENT) {
            Var* var = lookup_var(scope->vars, func->body.as_string);
            // NOTE: Is this the right `offset`?
            EXIT_IF_LOC(var, memory, func->offset);
            push_var(memory, &scope->vars, func->body.as_string, arg);
        } else {
            EXIT_LOC(memory, func->offset);
        }
        return arg;
    }
    case INTRIN_UPDATE: {
        func = func->body.as_intrinsic.expr;
        if (func->tag == EXPR_CALL) {
            EXIT_IF_LOC(func->body.as_call.func->tag != EXPR_INTRIN,
                        memory,
                        func->offset);
            EXIT_IF_LOC(func->body.as_call.func->body.as_intrinsic.tag !=
                            INTRIN_ACCESS,
                        memory,
                        func->offset);
            Expr* obj =
                eval_expr(memory,
                          scope,
                          func->body.as_call.func->body.as_intrinsic.expr)
                    .expr;
            EXIT_IF_LOC(obj->tag != EXPR_OBJECT, memory, obj->offset);
            Expr* field =
                eval_expr(memory, scope, func->body.as_call.arg).expr;
            EXIT_IF_LOC(field->tag != EXPR_STRING, memory, field->offset);
            Var* var = lookup_var(obj->body.as_object, field->body.as_string);
            EXIT_IF_LOC(!var, memory, field->offset);
            var->env = arg;
        } else if (func->tag == EXPR_IDENT) {
            Var* var = lookup_scope(scope, func->body.as_string);
            // NOTE: Is this the right `offset`?
            EXIT_IF_LOC(!var, memory, func->offset);
            var->env = arg;
        } else {
            EXIT_LOC(memory, func->offset);
        }
        return arg;
    }
    case INTRIN_ACCESS: {
        Expr* l = eval_expr(memory, scope, func->body.as_intrinsic.expr).expr;
        EXIT_IF_LOC(l->tag != EXPR_OBJECT, memory, l->offset);
        Expr* r = arg.expr;
        EXIT_IF_LOC(r->tag != EXPR_STRING, memory, r->offset);
        Var* var = lookup_var(l->body.as_object, r->body.as_string);
        // NOTE: Is this the right `offset`?
        EXIT_IF_LOC(!var, memory, r->offset);
        return var->env;
    }
    case INTRIN_PRINT: {
        // NOTE: Is this the right `offset`?
        EXIT_IF_LOC(arg.expr->tag != EXPR_STRING, memory, offset);
        print_string(arg.expr->body.as_string);
        return arg;
    }
    case INTRIN_TOSTRING: {
        // NOTE: Is this the right `offset`?
        EXIT_IF_LOC(arg.expr->tag != EXPR_I64, memory, offset);
        arg.expr =
            alloc_expr_string(memory,
                              i64_to_string(memory, arg.expr->body.as_i64),
                              offset);
        return arg;
    }
    case INTRIN_EQ: {
        BINOP_I64(==);
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
    case INTRIN_ERROR:
    default: {
        EXIT_LOC(memory, func->offset);
    }
    }
}

#undef BINOP_I64

static Env eval_expr_call(Memory* memory,
                          Scope*  scope,
                          Expr*   func,
                          Env     arg,
                          u32     offset) {
    EXIT_IF(!scope);
    EXIT_IF(!func);
    EXIT_IF(!arg.expr);
    EXIT_IF_LOC(func->next.expr, memory, func->next.expr->offset);
    EXIT_IF_LOC(arg.expr->next.expr, memory, arg.expr->next.expr->offset);
    switch (func->tag) {
    case EXPR_IDENT:
    case EXPR_CALL:
    case EXPR_IF_ELSE: {
        Env env_func = eval_expr(memory, scope, func);
        return eval_expr_call(memory,
                              env_func.scope,
                              env_func.expr,
                              arg,
                              func->offset);
    }
    case EXPR_FN0: {
        EXIT_IF_LOC(arg.expr->tag != EXPR_EMPTY, memory, arg.expr->offset);
        return eval_exprs(memory,
                          push_scope(memory, scope),
                          func->body.as_fn0.list);
    }
    case EXPR_FN1: {
        scope = push_scope(memory, scope);
        push_var(memory, &scope->vars, func->body.as_fn1.label, arg);
        return eval_exprs(memory, scope, func->body.as_fn1.list);
    }
    case EXPR_INTRIN: {
        return eval_expr_intrinsic(memory, scope, func, arg, offset);
    }
    case EXPR_OBJECT:
    case EXPR_I64:
    case EXPR_STRING:
    case EXPR_EMPTY:
    case EXPR_ERROR:
    default: {
        EXIT_LOC(memory, offset);
    }
    }
}

Env eval_expr(Memory* memory, Scope* scope, Expr* expr) {
    EXIT_IF(!scope);
    EXIT_IF(!expr);
    switch (expr->tag) {
    case EXPR_IDENT: {
        Var* var = lookup_scope(scope, expr->body.as_string);
        EXIT_IF_LOC(!var, memory, expr->offset);
        return var->env;
    }
    case EXPR_CALL: {
        return eval_expr_call(memory,
                              scope,
                              expr->body.as_call.func,
                              eval_expr(memory, scope, expr->body.as_call.arg),
                              expr->offset);
    }
    case EXPR_IF_ELSE: {
        EXIT_IF_LOC(expr->body.as_if.condition->next.expr,
                    memory,
                    expr->body.as_if.condition->next.expr->offset);
        EXIT_IF_LOC(expr->body.as_if.if_then->next.expr,
                    memory,
                    expr->body.as_if.if_then->next.expr->offset);
        EXIT_IF_LOC(expr->body.as_if.if_else->next.expr,
                    memory,
                    expr->body.as_if.if_else->next.expr->offset);
        Expr* condition =
            eval_expr(memory, scope, expr->body.as_if.condition).expr;
        EXIT_IF_LOC(condition->tag != EXPR_I64, memory, condition->offset);
        return eval_expr(memory,
                         scope,
                         condition->body.as_i64 ? expr->body.as_if.if_then
                                                : expr->body.as_if.if_else);
    }
    case EXPR_OBJECT: {
        return (Env){
            .scope = scope,
            .expr = alloc_expr_object(memory, expr->offset),
        };
    }
    case EXPR_I64:
    case EXPR_FN0:
    case EXPR_FN1:
    case EXPR_INTRIN:
    case EXPR_STRING:
    case EXPR_EMPTY: {
        return (Env){.scope = scope, .expr = expr};
    }
    case EXPR_ERROR:
    default: {
        EXIT_LOC(memory, expr->offset);
    }
    }
}

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
    Memory* memory = alloc_memory();
    memory->source_path = args[1];
    memory->source_string = path_to_string(args[1]);
    tokenize(memory);
    ExprList list = parse(memory);
    Scope*   scope = alloc_scope(memory);
    push_intrin(memory, scope, STRING("print"), INTRIN_PRINT);
    push_intrin(memory, scope, STRING("toString"), INTRIN_TOSTRING);
    eval_exprs(memory, scope, list);
    return OK;
}
