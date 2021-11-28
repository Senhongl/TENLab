#include "tensor.h"

/**
 * sample: [[1.1,2.0],[3.5,4.2],[3.9, 0.6]] // 2.0
 * output:
 *  0  1
 *  1  2
 *  1  0
 *  [ CPUFloatType{3,2} ]
 */

torch::Tensor floordivide_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    std::string rounding_mode = "floor";
    torch::Tensor z_t = torch::div(x_t, y_t.item(), rounding_mode);
    return z_t;
}

extern "C" void *floordivide(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor");

    return (void *)fromTensor(floordivide_t(toTensor(x), toTensor(y)));
}