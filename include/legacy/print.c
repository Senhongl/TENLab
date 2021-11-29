#include "tensor.h"

void print_int(void *a, int m, int n)
{
    int *x = (int *)a, i, j;
    for (i = 0; i < m; i++) {
        for (j = 0; j < n; j++)
            if (j == n-1)
                printf("%d", x[i*n+j]);
            else
                printf("%d, ", x[i*n+j]);
        printf("\n");
    }
}

void print_float(void *a, int m, int n)
{
    float *x = (float *)a;
    int i, j;
    for (i = 0; i < m; i++) {
        for (j = 0; j < n; j++)
            if (j == n-1)
                printf("%f", x[i*n+j]);
            else
                printf("%f, ", x[i*n+j]);
        printf("\n");
    }
}

void print(void *a)
{
    tensor *x = (tensor *)a;

    if (x->ndim > 2)
        die("Not support %d, Only support less than 2-dim", x->ndim);
    
    int m, n;
    if (x->ndim == 2) {
        m = (int)x->dims[0];
        n = (int)x->dims[1];
    } else if (x->ndim == 1) {
        m = (int)x->dims[0];
    } else {
        m = 1;
    }
    

    switch (x->type)
    {
        case 0: print_int(x->data, m, n); break;
        case 1: print_float(x->data, m, n); break;

        default: die("Invalid type");
    }
}