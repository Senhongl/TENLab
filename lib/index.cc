#include "tensor.h"

torch::Tensor index_get_t(const torch::Tensor &x_t, 
    const c10::ArrayRef<torch::indexing::TensorIndex> &y_t)
{
    return x_t.index(y_t);
}

tensor *index_get_v(tensor *x, tensor *y)
{
    int8_t dim = y->dims[0];

    check(dim == 1, "Invalid index of vartensor");

    tensor *index = ((tensor **)y->data)[0];
    tensor **data = (tensor **)x->data;
    check(index->type == 0 || index->type == 4, "Index must be integer");
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

    std::vector<torch::indexing::TensorIndex> tmplist;

    tensor **indices = (tensor **)indx->data;
    for (int i = 0; i < dim; i++) {
        if (indices[i]->type == 4) {
            tmplist.push_back(torch::indexing::Slice
                (indices[i]->dims[1], indices[i]->dims[2], indices[i]->dims[3]));
            continue;
        }
        check(indices[i]->type == 0, "Index must be integer");
        tmplist.push_back(toTensor(indices[i]).to(torch::kInt64));
    }
    c10::ArrayRef<torch::indexing::TensorIndex> indlist(tmplist);

    return (void *)fromTensor(index_get_t(toTensor(tenx), indlist));
}

extern "C" void *index_get_int(void *tena, int inda)
{
    tensor *indx = (tensor *)malloc(sizeof(tensor));
    indx->type = 0;
    indx->ndim = 1;
    indx->dims = (int8_t *)malloc(sizeof(int8_t));
    indx->dims[0] = 1;

    indx->data = (void *) malloc(sizeof(int));
    memcpy(indx->data, &inda, sizeof(int));
    
    tensor *indx2 = (tensor *)malloc(sizeof(tensor));
    tensor **tmp = (tensor **)malloc(sizeof(tensor *));
    tmp[0] = indx;
    indx2->type = 3;
    indx2->ndim = 1;
    indx2->dims = (int8_t *)malloc(sizeof(int8_t));
    indx2->dims[0] = 1;

    indx2->data = (void *)tmp;
    
    return index_get(tena, (void *)indx2);
}

void index_put_t(torch::Tensor x_t, 
    const c10::ArrayRef<torch::indexing::TensorIndex> &y_t, 
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
    check(index->type == 0, "Index must be integer");
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

    std::vector<torch::indexing::TensorIndex> tmplist;

    tensor **indices = (tensor **)indx->data;
    for (int i = 0; i < dim; i++) {
        if (indices[i]->type == 4) {
            tmplist.push_back(torch::indexing::Slice
                (indices[i]->dims[1], indices[i]->dims[2], indices[i]->dims[3]));
            continue;
        }
        check(indices[i]->type == 0, "Index must be integer");
        tmplist.push_back(toTensor(indices[i]).to(torch::kInt64));
    }
    c10::ArrayRef<torch::indexing::TensorIndex> indlist(tmplist);

    index_put_t(toTensor(tenx), indlist, toTensor(ntenx));
}