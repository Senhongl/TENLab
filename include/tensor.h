#ifndef __TENSOR_H
#define __TENSOR_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

#include <iostream>
#include <torch/torch.h>

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

torch::Dtype toType(const tensor * const a);
int8_t fromType(const torch::Dtype &a_type);
torch::Tensor toTensor(const tensor * const a);
tensor *fromTensor(const torch::Tensor &a_t);

#endif /* __TENSOR_H */