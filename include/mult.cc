#include "tensor.h"

torch::Tensor mult_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::matmul(x_t, y_t);
    return z_t;
}

extern "C" void *mult(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");

    return (void *)fromTensor(mult_t(toTensor(x), toTensor(y)));
}