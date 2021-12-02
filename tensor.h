#ifndef __TENSOR_H
#define __TENSOR_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

#define check(cond, ...) \
do {\
    if (cond)\
        break;\
    fprintf(stderr, __VA_ARGS__);\
    if (errno)\
        perror(" ");\
    exit(1);\
} while(0)

typedef struct tensor
{
    int8_t type;
    int8_t ndim;
    int8_t *dims;
    void *data;
} tensor;

#endif /* __TENSOR_H */