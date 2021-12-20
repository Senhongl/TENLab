#include "tensor.h"

torch::Tensor cat_t(const torch::Tensor &x_t, const torch::Tensor &y_t, const torch::Tensor &dim_t)
{
    torch::Tensor a_t = torch::cat({x_t, y_t}, dim_t.item().to<int64_t>());
    return a_t;
}

extern "C" void *cat(void *a, void *b, void *c)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;
    tensor *z = (tensor *)c;

    check(x->type == y->type, "Not consistent type");
    check(z->ndim == 0, "Second tensor should be 0-dim tensor");

    return (void *)fromTensor(cat_t(toTensor(x), toTensor(y), toTensor(z)));
}