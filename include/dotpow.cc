#include "tensor.h"

/**
 * sample: [[1.1,2.0],[3.5,4.2]] .^ 2
 * output:
 *  1.2100   4.0000
 *  12.2500  17.6400
 *  [ CPUFloatType{2,2} ]
 */

torch::Tensor dotpow_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    int64_t exp = y_t.item().to<int64_t>();
    torch::Tensor z_t = x_t;
    for (int i = 1; i < exp; i++)
        z_t = z_t * x_t;
    return z_t;
}

extern "C" void *dotpow(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(y->type == 0, "Second tensor should have int type");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor");

    return (void *)fromTensor(dotpow_t(toTensor(x), toTensor(y)));
}