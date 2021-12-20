#include "tensor.h"

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
        indlist.push_back(toTensor(indices[i]).to(torch::kInt64));
    }

    return (void *)fromTensor(index_get_t(toTensor(tenx), indlist));
}

void index_put_t(torch::Tensor x_t, 
    const c10::List<c10::optional<torch::Tensor>> &y_t, 
    torch::Tensor z_t)
{
    x_t.index_put_(y_t, z_t);
}

extern "C" void index_put(void *tena, void *inda, void *ntena)
{
    tensor *tenx = (tensor *)tena;
    tensor *indx = (tensor *)inda;
    tensor *ntenx = (tensor *)ntena;
    tensor *teny;
    int8_t dim = indx->dims[0];

    c10::List<c10::optional<torch::Tensor>> indlist;

    tensor **indices = (tensor **)indx->data;
    for (int i = 0; i < dim; i++) {
        indlist.push_back(toTensor(indices[i]).to(torch::kInt64));
    }

    index_put_t(toTensor(tenx), indlist, toTensor(ntenx));
}