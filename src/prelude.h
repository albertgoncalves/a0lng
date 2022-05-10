#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
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

typedef struct stat FileStat;

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

typedef struct {
    const char* buffer;
    u32         len;
} String;

#define STRING(literal)             \
    ((String){                      \
        .buffer = literal,          \
        .len = sizeof(literal) - 1, \
    })

static Bool eq(String a, String b) {
    return (a.len == b.len) && (!memcmp(a.buffer, b.buffer, a.len));
}

static void print_string(String string) {
    printf("%.*s", string.len, string.buffer);
}

static String path_to_string(const char* path) {
    EXIT_IF(!path);
    const i32 file = open(path, O_RDONLY);
    EXIT_IF(file < 0);
    FileStat stat;
    EXIT_IF(fstat(file, &stat) < 0)
    const String string = {
        .buffer =
            mmap(NULL, (u32)stat.st_size, PROT_READ, MAP_SHARED, file, 0),
        .len = (u32)stat.st_size,
    };
    EXIT_IF(string.buffer == MAP_FAILED);
    close(file);
    return string;
}

#endif
