#include "tensor.h"

/**
 * sample: zeros([3,2,3])
 */

torch::Tensor zeros_t(const torch::Tensor &x_t)
{
    int64_t size = x_t.sizes()[0];
    std::vector<int64_t> dims;
    for (int i = 0; i < size; i++) 
        dims.push_back(x_t[i].item().to<int64_t>());
    at::IntArrayRef ndims (dims);
    torch::Tensor z_t = torch::zeros(ndims);
    return z_t.toType(torch::kInt32);
}

extern "C" void *zeros(void *a)
{
    tensor *x = (tensor *)a;
    check(x->type == 0, "Not consistent type");
    check(x->ndim == 1 , "Dimension should be 1");
    return (void *)fromTensor(zeros_t(toTensor(x)));
}