/*#include "tensor.h"

torch::Tensor index_get_t(const torch::Tensor &x_t, 
    const c10::List<c10::optional<torch::Tensor>> &y_t)
{
    return x_t.index(y_t);
}

extern "C" void *index_get(void *tena, void *inda)
{
    tensor *tenx = (tensor *)tena;
    tensor *indx = (tensor *)inda;
    int8_t dim = indx->dims[0];

    c10::List<c10::optional<torch::Tensor>> indlist;

    tensor **indices = (tensor **)indx->data;
    for (int i = 0; i < dim; i++) {
        indlist.push_back(toTensor(indices[i]));
    }

    return (void *)fromTensor(index_get_t(toTensor(tenx), indlist));
}*/