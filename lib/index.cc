#include "tensor.h"

torch::Tensor index_get_t(const torch::Tensor &x_t, 
    const c10::List<c10::optional<torch::Tensor>> &y_t)
{
    return x_t.index(y_t);
}

tensor *index_get_v(tensor *x, tensor *y)
{
    int8_t dim = y->dims[0];

    check(dim == 1, "Invalid index of vartensor");

    tensor *index = ((tensor **)y->data)[0];
    tensor **data = (tensor **)x->data;
    int32_t *ind_data = (int32_t *)index->data;

    dim = index->ndim;
    check(dim == 1 || dim == 0, "Invalid index of vartensor");

    if (dim == 0) {
        check(ind_data[0] < x->dims[0], "Out of range");
        return data[ind_data[0]];
    }

    int8_t numInd = index->dims[0];
    tensor **ret_data = (tensor **)malloc(sizeof(tensor *) * numInd);
    dim = x->dims[0];
    for (int i = 0; i < numInd; i++) {
        check(ind_data[i] < dim, "Out of range");
        ret_data[i] = data[ind_data[i]];
    }

    tensor *ret_ten = (tensor *)malloc(sizeof(tensor));
    ret_ten->type = 3;
    ret_ten->ndim = 1;
    ret_ten->dims = (int8_t *)malloc(sizeof(int8_t));
    ret_ten->dims[0] = numInd;
    ret_ten->data = (void *)ret_data;
    return ret_ten;
}

extern "C" void *index_get(void *tena, void *inda)
{
    tensor *tenx = (tensor *)tena;
    tensor *indx = (tensor *)inda;
    int8_t dim = indx->dims[0];

    if (tenx->type == 3)
        return (void *)index_get_v(tenx, indx);

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

void index_put_v(tensor *x, tensor *y, tensor *z)
{
    int8_t dim = y->dims[0];

    check(dim == 1, "Invalid index of vartensor");

    tensor *index = ((tensor **)y->data)[0];
    tensor **data = (tensor **)x->data;
    int32_t *ind_data = (int32_t *)index->data;

    dim = index->ndim;
    check(dim == 1 || dim == 0, "Invalid index of vartensor");

    if (dim == 0) {
        check(ind_data[0] < x->dims[0], "Out of range");
        data[ind_data[0]] = z;
        return;
    }

    check(z->type == 3, "Invalid input");
    int8_t numInd = index->dims[0];
    check(numInd == z->dims[0], "Inconsistent input size");
    tensor **newdata = (tensor **)z->data;

    dim = x->dims[0];
    for (int i = 0; i < numInd; i++) {
        check(ind_data[i] < dim, "Out of range");
        data[ind_data[i]] = newdata[i];
    }
}

extern "C" void index_put(void *tena, void *inda, void *ntena)
{
    tensor *tenx = (tensor *)tena;
    tensor *indx = (tensor *)inda;
    tensor *ntenx = (tensor *)ntena;
    tensor *teny;
    int8_t dim = indx->dims[0];

    if (tenx->type == 3)
        return index_put_v(tenx, indx, ntenx);

    c10::List<c10::optional<torch::Tensor>> indlist;

    tensor **indices = (tensor **)indx->data;
    for (int i = 0; i < dim; i++) {
        indlist.push_back(toTensor(indices[i]).to(torch::kInt64));
    }

    index_put_t(toTensor(tenx), indlist, toTensor(ntenx));
}