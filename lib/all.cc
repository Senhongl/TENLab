#include "tensor.h"

torch::Tensor all_t(const torch::Tensor &x_t)
{
    torch::Tensor a_t = torch::all(x_t);
    return a_t;
}

extern "C" void *all(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(all_t(toTensor(x)));
}
