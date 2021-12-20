#include "tensor.h"

/**
 * sample: [[1.1,2.0],[3.5,4.2]] ^ 2
 *  8.2100  10.6000
 *  18.5500  24.6400
 *  [ CPUFloatType{2,2} ]
 */

torch::Tensor matpow_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::linalg::matrix_power(x_t, y_t.item().to<int64_t>());
    return z_t;
}

extern "C" void *matpow(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(y->type == 0, "Second tensor should have int type\n");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor\n");

    return (void *)fromTensor(matpow_t(toTensor(x), toTensor(y)));
}