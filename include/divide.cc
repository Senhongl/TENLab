#include "tensor.h"

/**
 * sample: [[1.1,2.0],[3.5,4.2]] / 0.5
 */

torch::Tensor divide_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t.div(y_t.item());
    return z_t;
}

extern "C" void *divide(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor");

    return (void *)fromTensor(divide_t(toTensor(x), toTensor(y)));
}