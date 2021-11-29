#include "tensor.h"

/**
 * sample: [[1.1,2.0],[3.5,4.2]] % 1.6
 * output:
 *   1.1000  0.4000
 *   0.3000  1.0000
 *   [ CPUFloatType{2,2} ]
 */

torch::Tensor mod_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::fmod(x_t, y_t.item());
    return z_t;
}

extern "C" void *mod(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Second tensor should have int type");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor");

    return (void *)fromTensor(mod_t(toTensor(x), toTensor(y)));
}