#include "tensor.h"

torch::Tensor shape_t(const torch::Tensor &x_t)
{
    c10::IntArrayRef a = x_t.sizes();
    int64_t size = a.size();
    int64_t dim_tmp[size];
    for (int i = 0; i < size; i++){
        dim_tmp[i] = (int64_t)(a[i]);
    }
    c10::IntArrayRef a_dim (size);
    auto options = torch::TensorOptions().dtype(torch::kInt32);
    torch::Tensor z_t = torch::from_blob(dim_tmp, a_dim, options);
    return z_t.clone();
}

extern "C" void *shape(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(shape_t(toTensor(x)));
}