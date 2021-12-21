#include "tensor.h"

torch::Tensor any_t(const torch::Tensor &x_t)
{
    torch::Tensor a_t = torch::any(x_t);
    return a_t;
}

extern "C" void *any(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(any_t(toTensor(x)));
}
