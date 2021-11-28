#include "tensor.h"
#include <array>
/**
 * sample: [[1.1,2.0],[3.5,4.2],[3.9, 0.6]] .* [[1.1,2.0],[3.5,4.2],[3.9, 0.6]]
 * output:
 *  1.2100   4.0000
 *  12.2500  17.6400
 *  15.2100   0.3600
 *  [ CPUFloatType{3,2} ]
 */

torch::Tensor dotmul_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t * y_t;
    return z_t;
}

extern "C" void *dotmul(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");
    check(x->ndim == y->ndim, "Not consistent dimension");

    return (void *)fromTensor(dotmul_t(toTensor(x), toTensor(y)));
}