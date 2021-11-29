#include "tensor.h"

torch::Tensor greater_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t > y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *greater(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");

    return (void *)fromTensor(greater_t(toTensor(x), toTensor(y)));
}