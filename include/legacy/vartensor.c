#include "tensor.h"

void __vartensor(tensor *x)
{
    if (x->type != 3) {
        printf("%d", *(int *)(x->data));
        return;
    }
    int dim = x->dims[0], i;
    tensor **data = (tensor **)x->data;
    printf("[");
    for (i = 0; i < dim; i++) {
        __vartensor(data[i]);
        if (i != dim-1)
            printf(",");
    }
    printf("]");
}

void print(void *a)
{
    tensor *x = (tensor *)a;
    __vartensor(x);
    printf("\n");
}
