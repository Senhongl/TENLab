#include "tensor.h"

/***
 * sample: [[1,2,3],[4,5,6]]'
 * output:
 *  1  2
 *  3  2
 *  3  4
 *  [ CPUIntType{3,2} ]
 ***/

torch::Tensor transpose_t(const torch::Tensor &x_t)
{
    torch::Tensor z_t = x_t.transpose(-1, 0);
    return z_t;
}

extern "C" void *transpose(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(transpose_t(toTensor(x)));
}
