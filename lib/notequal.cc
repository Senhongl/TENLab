#include "tensor.h"

torch::Tensor notequal_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t != y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *notequal(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");
    check(x->ndim == y->ndim, "Not consistent dimension\n");
    bool flag = true;
    for (int i = 0; i < x->ndim; i++) {
        if (x->dims[i] != y->dims[i]) {
            flag = false;
            break;
        }
    }
    check(flag == true, "Not consistent dimension\n");

    return (void *)fromTensor(notequal_t(toTensor(x), toTensor(y)));
}