#include "tensor.h"

void *add_int(void *a, void *b, int n)
{
    int i;
    int *x = (int *)a;
    int *y = (int *)b;
    int *z = (int *)malloc(n);
    for (i = 0; i < n; i++)
        z[i] = x[i] + y[i];
    return (void *)z;
}

void *add_float(void *a, void *b, int n)
{
    int i;
    float *x = (float *)a;
    float *y = (float *)b;
    float *z = (float *)malloc(n);
    for (i = 0; i < n; i++)
        z[i] = x[i] + y[i];
    return (void *)z;
}

void *add(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    if (x->type != y->type)
        die("Not consistent type");
    if (x->ndim != y->ndim)
        die("Not consistent ndim");
    
    int type = (int)x->type;
    int ndim = (int)x->ndim, i, numele = 1;
    int8_t *dims = (int8_t *)malloc(sizeof(int8_t)*ndim);
    for (i = 0; i < ndim; i++) {
        if (x->dims[i] != y->dims[i]) {
            free(dims);
            die("Not consistent dim %d", i);
        }
        numele *= (int)x->dims[i];
        dims[i] = x->dims[i];
    }

    void *data_z;
    switch (type)
    {
        case 0: data_z = add_int(x->data, y->data, numele); break;
        case 1: data_z = add_float(x->data, y->data, numele); break;
    
        default: free(dims); die("Invalid type");
    }

    tensor *z = (tensor *)malloc(sizeof(tensor));
    z->type = (int8_t)type;
    z->ndim = (int8_t)ndim;
    z->dims = dims;
    z->data = data_z;
    return (void *)z;
}