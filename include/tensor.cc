#include "tensor.h"

torch::Dtype toType(const tensor * const a)
{
    switch (a->type) {
        case 0: return torch::kInt32;
        case 1: return torch::kFloat32;

        default: check(0, "Invalid type");
    }
}

int8_t fromType(const torch::Dtype &a_type)
{
    switch (a_type) {
        case torch::kInt32: return 0;
        case torch::kFloat32: return 1;

        default: check(0, "Invalid type");
    }
}

torch::Tensor toTensor(const tensor * const a)
{
    int64_t *dim_tmp;

    dim_tmp = new int64_t[a->ndim];
    for (int i = 0; i < a->ndim; i++)
        dim_tmp[i] = (int64_t)(a->dims[i]);
    c10::IntArrayRef a_dim (dim_tmp, dim_tmp+a->ndim);

    torch::Tensor a_t = torch::from_blob(a->data, a_dim, toType(a));

    delete [] dim_tmp;

    return a_t;
}

tensor *fromTensor(const torch::Tensor &a_t)
{
    tensor *a = (tensor *)malloc(sizeof(tensor));

    a->type = fromType(a_t.scalar_type());
    a->ndim = (int8_t)a_t.dim();

    a->dims = (int8_t *)malloc(sizeof(int8_t)*a->ndim);
    for (int i = 0; i < a->ndim; i++)
        a->dims[i] = (int8_t)a_t.size(i);

    unsigned int eleBytes = sizeof(int32_t) * torch::numel(a_t);
    a->data = (int32_t *)malloc(eleBytes);
    memcpy(a->data, a_t.data_ptr(), eleBytes);

    return a;
}