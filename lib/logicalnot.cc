#include "tensor.h"

/**
 * sample: ![[1,0,1],[0,1,0]]
 * output:
 * 0  1  0
 * 1  0  1
 * [ CPUIntType{2,3} ]
 */


torch::Tensor logicalnot_t(const torch::Tensor &x_t)
{
    torch::Tensor z_t = torch::logical_not(x_t);
    return z_t.toType(torch::kInt32);
}

extern "C" void *logicalnot(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(logicalnot_t(toTensor(x)));
}