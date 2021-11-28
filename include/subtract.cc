#include "tensor.h"

/**
 * sample: [[1.1,2.0],[3.5,4.2]] / 0.5
 */

torch::Tensor subtract_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t - y_t;
    return z_t;
}

extern "C" void *subtract(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");

    return (void *)fromTensor(subtract_t(toTensor(x), toTensor(y)));
}