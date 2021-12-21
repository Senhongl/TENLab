#include "tensor.h"

/**
 * sample: [[1.1,2.0],[3.5,4.2]] - [[0.1,0.3],[2.2,6.2]]
 */

torch::Tensor subtract_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t - y_t;
    return z_t;
}

torch::Tensor negative_t(const torch::Tensor &x_t)
{
    return torch::neg(x_t);
}

extern "C" void *subtract(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");
    check(x->ndim == y->ndim, "Not consistent dimension\n");
    bool flag = true;
    for (int i = 0; i < x->ndim; i++) {
        if (x->dims[i] != y->dims[i]) {
            flag = false;
            break;
        }
    }
    check(flag == true, "Not consistent dimension\n");

    return (void *)fromTensor(subtract_t(toTensor(x), toTensor(y)));
}

extern "C" void *negative(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(negative_t(toTensor(x)));
}