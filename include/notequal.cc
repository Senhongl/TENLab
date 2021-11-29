#include "tensor.h"

torch::Tensor notequal_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t != y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *notequal(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");

    return (void *)fromTensor(notequal_t(toTensor(x), toTensor(y)));
}