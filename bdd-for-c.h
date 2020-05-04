#pragma clang diagnostic push
#pragma ide diagnostic ignored "cert-dcl51-cpp"
#pragma ide diagnostic ignored "cert-dcl37-c"
#pragma ide diagnostic ignored "bugprone-reserved-identifier"
/*!
The MIT License (MIT)

Copyright (c) 2016 Dmitriy Kubyshkin <dmitriy@kubyshkin.name>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef BDD_FOR_C_H
#define BDD_FOR_C_H

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#ifndef _WIN32
#include <unistd.h>
#include <term.h>

#define __BDD_IS_ATTY__() isatty(fileno(stdout))
#else
#include <Windows.h>
#include <io.h>
#define __BDD_IS_ATTY__() _isatty(_fileno(stdout))
#endif

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4996) // _CRT_SECURE_NO_WARNINGS
#endif

#ifndef BDD_USE_COLOR
#define BDD_USE_COLOR 1
#endif

#ifndef BDD_USE_TAP
#define BDD_USE_TAP 0
#endif

#define __BDD_COLOR_RESET__       "\x1B[0m"
#define __BDD_COLOR_RED__         "\x1B[31m"             /* Red */
#define __BDD_COLOR_GREEN__       "\x1B[32m"             /* Green */
#define __BDD_COLOR_BOLD__        "\x1B[1m"              /* Bold White */

size_t __bdd_strlcat__(char* dst, const char* src, size_t size)
{
    size_t len = 0;
    size_t slen = strlen(src);
    while (*dst && size > 0) {
        dst++;
        len++;
        size--;
    }
    while (*src && size-- > 1) {
        *dst++ = *src++;
    }
    if (size == 1 || *src == 0) {
        *dst = 0;
    }
    return slen + len;
}


bool __bdd_same_string__(const char *str1, const char *str2) {
    size_t str1length = strlen(str1);
    size_t str2length = strlen(str2);
    if (str1length != str2length) {
        return 0;
    }
    return strncmp(str1, str2, str1length) == 0;
}

typedef struct __bdd_array__ {
    void **values;
    size_t capacity;
    size_t size;
} __bdd_array__;

__bdd_array__ *__bdd_array_create__() {
    __bdd_array__ *arr = malloc(sizeof(__bdd_array__));
    if (!arr) {
        perror("malloc(array)");
        abort();
    }
    arr->capacity = 4;
    arr->size = 0;
    arr->values = calloc(arr->capacity, sizeof(void *));
    return arr;
}

void *__bdd_array_push__(__bdd_array__ *arr, void *item) {
    if (arr->size == arr->capacity) {
        arr->capacity *= 2;
        void *v = realloc(arr->values, sizeof(void*) * arr->capacity);
        if (!v) {
            perror("realloc(array)");
            abort();
        }
        arr->values = v;
    }
    arr->values[arr->size++] = item;
    return item;
}

void *__bdd_array_last__(__bdd_array__ *arr) {
    if (arr->size == 0) {
        return NULL;
    }
    return arr->values[arr->size - 1];
}

void *__bdd_array_pop__(__bdd_array__ *arr) {
    if (arr->size == 0) {
        return NULL;
    }
    void *result = arr->values[arr->size - 1];
    --arr->size;
    return result;
}

void __bdd_array_free__(__bdd_array__ *arr) {
    free(arr->values);
    free(arr);
}

typedef enum __bdd_node_type__ {
    __BDD_NODE_GROUP__ = 1,
    __BDD_NODE_TEST__ = 2,
    __BDD_NODE_INTERIM__ = 3
} __bdd_node_type__;

typedef struct __bdd_test_step__ {
    size_t level;
    char *name;
    char *full_name;
    __bdd_node_type__ type;
} __bdd_test_step__;

typedef struct __bdd_node__ {
    char *name;
    char *prefix;
    __bdd_node_type__ type;
    __bdd_array__ *list_before;
    __bdd_array__ *list_after;
    __bdd_array__ *list_before_each;
    __bdd_array__ *list_after_each;
    __bdd_array__ *list_children;
} __bdd_node__;

__bdd_test_step__ *__bdd_test_step_create__(size_t level, __bdd_node__ *node) {
    __bdd_test_step__ *step = malloc(sizeof(__bdd_test_step__));
    if (!step) {
        perror("malloc(step)");
        abort();
    }
    step->level = level;
    step->type = node->type;

    size_t fullname_len = strlen(node->prefix) + strlen(node->name) + 1;

    step->full_name = calloc(fullname_len, sizeof(char));
    if (!step->full_name) {
        perror("calloc(full_name)");
        abort();
    }
    __bdd_strlcat__(step->full_name, node->prefix, fullname_len);
    __bdd_strlcat__(step->full_name, node->name, fullname_len);

    step->name = node->name;
    return step;
}

__bdd_node__ *__bdd_node_create__(char *name, char *prefix, __bdd_node_type__ type) {
    __bdd_node__ *n = malloc(sizeof(__bdd_node__));
    if (!n) {
        perror("malloc(node)");
        abort();
    }
    n->name = name;
    n->prefix = prefix;
    n->type = type;
    n->list_before = __bdd_array_create__();
    n->list_after = __bdd_array_create__();
    n->list_before_each = __bdd_array_create__();
    n->list_after_each = __bdd_array_create__();
    n->list_children = __bdd_array_create__();
    return n;
}

bool __bdd_node_is_leaf__(__bdd_node__ *node) {
    return node->list_children->size == 0;
}

void __bdd_node_flatten_internal__(
    size_t level,
    __bdd_node__ *node,
    __bdd_array__  *steps,
    __bdd_array__  *before_each_lists,
    __bdd_array__  *after_each_lists
) {
    if (__bdd_node_is_leaf__(node)) {

        for (size_t listIndex = 0; listIndex < before_each_lists->size; ++listIndex) {
            __bdd_array__ *list = before_each_lists->values[listIndex];
            for (size_t i = 0; i < list->size; ++i) {
                __bdd_array_push__(steps, __bdd_test_step_create__(level, list->values[i]));
            }
        }

        __bdd_array_push__(steps, __bdd_test_step_create__(level, node));

        for (size_t listIndex = 0; listIndex < after_each_lists->size; ++listIndex) {
            __bdd_array__ *list = after_each_lists->values[listIndex];
            for (size_t i = 0; i < list->size; ++i) {
                __bdd_array_push__(steps, __bdd_test_step_create__(level, list->values[i]));
            }
        }
        return;
    }

    __bdd_array_push__(steps, __bdd_test_step_create__(level, node));

    for (size_t i = 0; i < node->list_before->size; ++i) {
        __bdd_array_push__(steps, __bdd_test_step_create__(level, node->list_before->values[i]));
    }

    __bdd_array_push__(before_each_lists, node->list_before_each);
    __bdd_array_push__(after_each_lists, node->list_after_each);

    for (size_t i = 0; i < node->list_children->size; ++i) {
        __bdd_node_flatten_internal__(level + 1, node->list_children->values[i], steps, before_each_lists, after_each_lists);
    }

    __bdd_array_pop__(before_each_lists);
    __bdd_array_pop__(after_each_lists);

    for (size_t i = 0; i < node->list_after->size; ++i) {
        __bdd_array_push__(steps, __bdd_test_step_create__(level, node->list_after->values[i]));
    }
}

__bdd_array__ *__bdd_node_flatten__(__bdd_node__ *node, __bdd_array__ *names) {
    if (node == NULL) {
        return names;
    }

    __bdd_node_flatten_internal__(0, node, names, __bdd_array_create__(), __bdd_array_create__());

    return names;
}

void __bdd_node_free__(__bdd_node__ *n);

void __bdd_node_free_list__(__bdd_array__ *list) {
    for (size_t i = 0; i < list->size; ++i) {
        __bdd_node_free__(list->values[i]);
    }
    __bdd_array_free__(list);
}

void __bdd_node_free__(__bdd_node__ *n) {
    __bdd_node_free_list__(n->list_before);
    __bdd_node_free_list__(n->list_after);
    __bdd_node_free_list__(n->list_before_each);
    __bdd_node_free_list__(n->list_after_each);
    __bdd_node_free_list__(n->list_children);
}

char *__bdd_node_names_concat__(__bdd_array__ *list, const char *delimiter) {
    size_t result_size = 1;

    for (size_t i = 0; i < list->size; ++i) {
        result_size += strlen(((__bdd_node__ *) list->values[i])->name) + strlen(delimiter);
    }

    char *result = calloc(result_size, sizeof(char));
    if (!result) {
        perror("calloc(result)");
        abort();
    }

    for (size_t i = 0; i < list->size; ++i) {
        __bdd_strlcat__(result, ((__bdd_node__ *) list->values[i])->name, result_size);
        __bdd_strlcat__(result, delimiter, result_size);
    }

    return result;
}

enum __bdd_run_type__ {
    __BDD_INIT_RUN__ = 1,
    __BDD_TEST_RUN__ = 2
};

typedef struct __bdd_config_type__ {
    enum __bdd_run_type__ run;
    size_t test_index;
    size_t test_tap_index;
    size_t failed_test_count;
    __bdd_test_step__ *current_test;
    __bdd_array__ *node_stack;
    char *error;
    char *location;
    bool use_color;
    bool use_tap;
} __bdd_config_type__;

char *__bdd_spec_name__;
void __bdd_test_main__(__bdd_config_type__ *__bdd_config__);

void __bdd_run__(__bdd_config_type__ *config) {
    __bdd_test_step__ *step = config->current_test;
    __bdd_test_main__(config);

    if (step->type == __BDD_NODE_GROUP__ && !config->use_tap) {
        for (size_t i = 0; i < step->level; ++i) {
            printf("  ");
        }
        printf(
            "%s%s%s\n",
            config->use_color ? __BDD_COLOR_BOLD__ : "",
            step->name,
            config->use_color ? __BDD_COLOR_RESET__ : ""
        );
    }

    if (step->type != __BDD_NODE_TEST__) {
        return;
    }

    ++config->test_tap_index;

    if (config->error == NULL) {
        if (config->run == __BDD_TEST_RUN__) {
            if (config->use_tap) {
                // We only to report tests and not setup / teardown success
                if (config->test_tap_index) {
                    printf("ok %zu - %s\n", config->test_tap_index, step->name);
                }
            } else {
                for (size_t i = 0; i < step->level; ++i) {
                    printf("  ");
                }
                printf(
                    "%s %s(OK)%s\n", step->name,
                    config->use_color ? __BDD_COLOR_GREEN__ : "",
                    config->use_color ? __BDD_COLOR_RESET__ : ""
                );
            }
        }
    } else {
        ++config->failed_test_count;
        if (config->use_tap) {
            // We only to report tests and not setup / teardown errors
            if (config->test_tap_index) {
                printf("not ok %zu - %s\n", config->test_tap_index, step->name);
            }
        } else {
            for (size_t i = 0; i < step->level; ++i) {
                printf("  ");
            }
            printf(
                "%s %s(FAIL)%s\n", step->name,
                config->use_color ? __BDD_COLOR_RED__ : "",
                config->use_color ? __BDD_COLOR_RESET__ : ""
            );
            for (size_t i = 0; i < step->level + 1; ++i) {
                printf("  ");
            }
            printf("%s\n", config->error);
            for (size_t i = 0; i < step->level + 2; ++i) {
                printf("  ");
            }
            printf("%s\n", config->location);
        }
        free(config->error);
        config->error = NULL;
    }
}

char *__bdd_format__(const char *format, ...) {
    va_list va = 0;
    va_start(va, format);

    // First we over-allocate
    const size_t size = 2048;
    char *result = calloc(size, sizeof(char));
    if (!result) {
        perror("calloc(result)");
        abort();
    }
    vsnprintf(result, size - 1, format, va);

    // Then clip to an actual size
    void* r = realloc(result, strlen(result) + 1);
    if (!r) {
        perror("realloc(result)");
        abort();
    }
    result = r;

    va_end(va);
    return result;
}

bool __bdd_is_supported_term__() {
    bool result = false;
    const char *term = getenv("TERM");
    result = term && strcmp(term, "") != 0;
#ifndef _WIN32
    return result;
#else
    if (result) {
        return 1;
    }

    // Attempt to enable virtual terminal processing on Windows.
    // See: https://msdn.microsoft.com/en-us/library/windows/desktop/mt638032(v=vs.85).aspx
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut == INVALID_HANDLE_VALUE) {
        return 0;
    }

    DWORD dwMode = 0;
    if (!GetConsoleMode(hOut, &dwMode)) {
        return 0;
    }

    dwMode |= 0x4; // ENABLE_VIRTUAL_TERMINAL_PROCESSING
    if (!SetConsoleMode(hOut, dwMode)) {
        return 0;
    }

    return 1;
#endif
}

int main(void) {
    struct __bdd_config_type__ config = {
        .run = __BDD_INIT_RUN__,
        .test_index = 0,
        .test_tap_index = 0,
        .failed_test_count = 0,
        .node_stack = __bdd_array_create__(),
        .error = NULL,
        .use_color = 0,
        .use_tap = 0
    };

    const char *tap_env = getenv("BDD_USE_TAP");
    if (BDD_USE_TAP || (tap_env && strcmp(tap_env, "") != 0 && strcmp(tap_env, "0") != 0)) {
        config.use_tap = 1;
    }

    if (!config.use_tap && BDD_USE_COLOR && __BDD_IS_ATTY__() && __bdd_is_supported_term__()) {
        config.use_color = 1;
    }

    __bdd_array_push__(config.node_stack, __bdd_node_create__(__bdd_spec_name__, "", __BDD_NODE_GROUP__));

    // During the first run we just gather the
    // count of the tests and their descriptions
    __bdd_test_main__(&config);

    __bdd_array__ *steps = __bdd_array_create__();
    __bdd_node_flatten__(config.node_stack->values[0], steps);

    size_t test_count = 0;
    for (size_t i = 0; i < steps->size; ++i) {
        __bdd_test_step__ *step = steps->values[i];
        if(step->type == __BDD_NODE_TEST__) {
            ++test_count;
        }
    }

    // Outputting the name of the suite
    if (config.use_tap) {
        printf("TAP version 13\n1..%zu\n", test_count);
    }

    config.run = __BDD_TEST_RUN__;

    for (size_t i = 0; i < steps->size; ++i) {
        __bdd_test_step__ *step = steps->values[i];
        __bdd_node_free__(config.node_stack->values[0]);
        config.node_stack->size = 0;
        __bdd_array_push__(config.node_stack, __bdd_node_create__(__bdd_spec_name__, "", __BDD_NODE_GROUP__));
        config.current_test = step;
        __bdd_run__(&config);
    }

    if (config.failed_test_count > 0) {
        if (!config.use_tap) {
            printf(
                "\n%zu test%s run, %zu failed.\n",
                test_count, test_count == 1 ? "" : "s", config.failed_test_count
            );
        }
        return 1;
    }

    return 0;
}

#define spec(name) \
char *__bdd_spec_name__ = (name);\
void __bdd_test_main__ (__bdd_config_type__ *__bdd_config__)\

#define __BDD_LAST_NODE__ ((__bdd_node__ *) __bdd_array_last__(__bdd_config__->node_stack))

#define __BDD_STEP__(node_list, node_name, type, delimiter)\
for(\
    void *__bdd_index__ = 0,\
         *__bdd_node_name__ = (node_name);\
    (\
        (\
            __bdd_config__->run == __BDD_INIT_RUN__ &&\
            __bdd_array_push__(\
                (node_list),\
                __bdd_node_create__(__bdd_node_name__, __bdd_node_names_concat__(__bdd_config__->node_stack, (delimiter)), (type))\
            ) &&\
            false \
        ) || \
        (\
            __bdd_config__->run == __BDD_TEST_RUN__ &&\
            (intptr_t) __bdd_index__ < 1 &&\
            __bdd_same_string__(\
                __bdd_format__("%s%s", __bdd_node_names_concat__(__bdd_config__->node_stack, (delimiter)), __bdd_node_name__),\
                __bdd_config__->current_test->full_name\
            )\
        )\
    );\
    __bdd_index__ = (char*)__bdd_index__ + 1\
)

#define it(name) __BDD_STEP__(\
  __BDD_LAST_NODE__->list_children,\
  name,\
  __BDD_NODE_TEST__,\
  "-#-it-#-"\
)

#define before_each() __BDD_STEP__(\
  __BDD_LAST_NODE__->list_before_each,\
  __bdd_format__("%i", __BDD_LAST_NODE__->list_before_each->size),\
  __BDD_NODE_INTERIM__,\
  "-#-before-each-#-"\
)

#define after_each() __BDD_STEP__(\
  __BDD_LAST_NODE__->list_after_each,\
  __bdd_format__("%i", __BDD_LAST_NODE__->list_after_each->size),\
  __BDD_NODE_INTERIM__,\
  "-#-after-each-#-"\
)

#define before() __BDD_STEP__(\
  __BDD_LAST_NODE__->list_before,\
  __bdd_format__("%i", __BDD_LAST_NODE__->list_before->size),\
  __BDD_NODE_INTERIM__,\
  "-#-before-#-"\
)

#define after() __BDD_STEP__(\
  __BDD_LAST_NODE__->list_after,\
  __bdd_format__("%i", __BDD_LAST_NODE__->list_after->size),\
  __BDD_NODE_INTERIM__,\
  "-#-after-#-"\
)

#define describe(name)\
for(\
    void *__bdd_index__ = 0,\
         *__bdd_current_node__ = __bdd_node_create__(\
            (name),\
            __bdd_node_names_concat__(__bdd_config__->node_stack, "-#-describe-#-"),\
            __BDD_NODE_GROUP__\
         )\
    ;\
    (\
        (intptr_t) __bdd_index__ < 1 && (\
            __bdd_array_push__(__BDD_LAST_NODE__->list_children, __bdd_current_node__),\
            __bdd_array_push__(__bdd_config__->node_stack, __bdd_current_node__),\
            true\
        )\
    );\
    ++__bdd_index__, __bdd_array_pop__(__bdd_config__->node_stack) \
)

#ifndef BDD_NO_CONTEXT_KEYWORD
#define context(name) describe(name)
#endif

#define __BDD_MACRO__(M, ...) __BDD_OVERLOAD__(M, __BDD_COUNT_ARGS__(__VA_ARGS__)) (__VA_ARGS__)
#define __BDD_OVERLOAD__(macro_name, suffix) __BDD_EXPAND_OVERLOAD__(macro_name, suffix)
#define __BDD_EXPAND_OVERLOAD__(macro_name, suffix) macro_name##suffix

#define __BDD_COUNT_ARGS__(...) __BDD_PATTERN_MATCH__(__VA_ARGS__,_,_,_,_,_,_,_,_,_,ONE__)
#define __BDD_PATTERN_MATCH__(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,N, ...) N

void __bdd_snprintf__(char *buffer, size_t bufflen, const char *fmt, const char *message) {
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4996) // _CRT_SECURE_NO_WARNINGS
#pragma warning(disable: 4774) // _CRT_SECURE_NO_WARNINGS
#endif
    snprintf(buffer, bufflen, fmt, message);
#ifdef _MSC_VER
#pragma warning(pop)
#endif
}

#define __BDD_STRING_HELPER__(x) #x
#define __BDD_STRING__(x) __BDD_STRING_HELPER__(x)
#define __STRING__LINE__ __BDD_STRING__(__LINE__)

#define __BDD_CHECK__(condition, ...) if (!(condition))\
{\
    const char *message = __bdd_format__(__VA_ARGS__);\
    const char *fmt = __bdd_config__->use_color ?\
        (__BDD_COLOR_RED__ "Check failed:" __BDD_COLOR_RESET__ " %s" ) :\
        "Check failed: %s";\
    __bdd_config__->location = "at " __FILE__ ":" __STRING__LINE__;\
    size_t bufflen = strlen(fmt) + strlen(message) + 1;\
    __bdd_config__->error = calloc(bufflen, sizeof(char));\
    __bdd_snprintf__(__bdd_config__->error, bufflen, fmt, message);\
    return;\
}

#define __BDD_CHECK_ONE__(condition) __BDD_CHECK__(condition, #condition)

#define check(...) __BDD_MACRO__(__BDD_CHECK_, __VA_ARGS__)

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif //BDD_FOR_C_H

#pragma clang diagnostic pop