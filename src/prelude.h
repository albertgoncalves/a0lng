#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

typedef uint32_t u32;
typedef int32_t  i32;
typedef int64_t  i64;

#define OK    0
#define ERROR 1

#define EXIT()                                              \
    {                                                       \
        printf("%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        _exit(ERROR);                                       \
    }

#define EXIT_IF(condition)                                                   \
    if (condition) {                                                         \
        printf("%s:%s:%d `%s`\n", __FILE__, __func__, __LINE__, #condition); \
        _exit(ERROR);                                                        \
    }

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

typedef struct {
    const char* buffer;
    u32         len;
} String;

#define STRING(literal)                \
    ((String){                         \
        .buffer = literal,             \
        .len    = sizeof(literal) - 1, \
    })

static Bool eq(String a, String b) {
    return (a.len == b.len) && (!memcmp(a.buffer, b.buffer, a.len));
}

static void print_string(String string) {
    printf("%.*s", string.len, string.buffer);
}

#endif
