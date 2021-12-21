#include "tensor.h"

extern "C" void *range(void *a, void *b, void *c)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;
    tensor *z = (tensor *)c;

    check(x->type == 0 && y->type == 0 && z->type == 0, "Slice must be integer");
    check(x->ndim == 0 && y->ndim == 0 && z->ndim == 0, "Slice must be integer");

    int indx = *(int *)x->data;
    int indy = *(int *)y->data;
    int indz = *(int *)z->data;

    return (void *)fromTensor(torch::arange(indx, indy, indz).to(torch::kInt32));
}