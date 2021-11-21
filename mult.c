#include "tensor.h"

void *mult(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    if (x->type != y->type)
        die("Not consistent type");
    if (x->ndim != y->ndim)
        die("Not consistent ndim");
    
    //int type = (int)x->type;
    int ndim = (int)x->ndim, i;
    for (i = 0; i < ndim; i++) {
        if (x->dims[i] != y->dims[i])
            die("Not consistent dim %d", i);
    }
}