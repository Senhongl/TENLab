#include "tensor.h"

int32_t data[6] = {1,2,3,4,5,6};
int8_t dim[2] = {2,3};
int8_t ndim = 2;
int8_t type = 0;
tensor t = {type, ndim, dim, (void *)data};
void *tena = &t;

torch::Tensor index_get_t(const torch::Tensor &x_t, 
    const c10::List<c10::optional<torch::Tensor>> &y_t)
{
    return x_t.index(y_t);
}

void index_get(void *inda)
{
    tensor *tenx = (tensor *)tena;
    tensor *indx = (tensor *)inda;
    int8_t dim = indx->dims[0];

    c10::List<c10::optional<torch::Tensor>> indlist;

    tensor **indices = (tensor **)indx->data;
    for (int i = 0; i < dim; i++) {
        indlist.push_back(toTensor(indices[i]).to(torch::kInt64));
    }

    std::cout << index_get_t(toTensor(tenx), indlist) << std::endl;
}

void print_var(tensor *x)
{
    if (x->type != 3) {
        std::cout << toTensor(x) << std::endl;
        return;
    }
    int8_t dim = x->dims[0], i;
    tensor **data = (tensor **)x->data;
    printf("[");
    for (i = 0; i < dim; i++) {
        print_var(data[i]);
        if (i != dim-1)
            printf(",");
    }
    printf("]");
}

extern "C" void print(void *a)
{
    tensor *x = (tensor *)a;
    if (x->type == 3) {
        print_var(x);
        // index_get(a);
    }
    else
        std::cout << toTensor(x) << std::endl;
}
