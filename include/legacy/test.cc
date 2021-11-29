#include <torch/torch.h>
#include <iostream>
#include "tensor.h"

void *add(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    int64_t *dim_x, *dim_y;

    dim_x = new int64_t[x->ndim];
    for (int i = 0; i < x->ndim; i++)
        dim_x[i] = (int64_t)(x->dims[i]);
    c10::IntArrayRef x_dim (dim_x, x->ndim);

    dim_y = new int64_t[y->ndim];
    for (int i = 0; i < y->ndim; i++)
        dim_y[i] = (int64_t)(y->dims[i]);
    c10::IntArrayRef y_dim (dim_y, y->ndim);

    torch::Tensor x_t = torch::from_blob(x->data, x_dim, torch::kInt32);
    torch::Tensor y_t = torch::from_blob(y->data, y_dim, torch::kInt32);
    torch::Tensor z_t = x_t + y_t;

    delete [] dim_x;
    delete [] dim_y;

    tensor *z = new tensor;
    z->type = x->type;
    z->ndim = (int8_t)z_t.dim();
    z->dims = (int8_t *)malloc(sizeof(int8_t)*x->ndim);
    for (int i = 0; i < z->ndim; i++)
        z->dims[i] = (int8_t)z_t.size(i);
    unsigned int eleBytes = sizeof(int32_t)*torch::numel(z_t);
    z->data = (int32_t *)malloc(eleBytes);
    memcpy(z->data, z_t.data_ptr(), eleBytes);

    return (void *)z;
}

void *mult(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    int64_t *dim_x, *dim_y;

    dim_x = new int64_t[x->ndim];
    for (int i = 0; i < x->ndim; i++)
        dim_x[i] = (int64_t)(x->dims[i]);
    c10::IntArrayRef x_dim (dim_x, x->ndim);

    dim_y = new int64_t[y->ndim];
    for (int i = 0; i < y->ndim; i++)
        dim_y[i] = (int64_t)(y->dims[i]);
    c10::IntArrayRef y_dim (dim_y, y->ndim);

    torch::Tensor x_t = torch::from_blob(x->data, x_dim, torch::kInt32);
    torch::Tensor y_t = torch::from_blob(y->data, y_dim, torch::kInt32);
    torch::Tensor z_t = torch::matmul(x_t, y_t);

    tensor *z = new tensor;
    z->type = x->type;
    z->ndim = (int8_t)z_t.dim();
    z->dims = (int8_t *)malloc(sizeof(int8_t)*x->ndim);
    for (int i = 0; i < z->ndim; i++)
        z->dims[i] = (int8_t)z_t.size(i);
    unsigned int eleBytes = sizeof(int32_t)*torch::numel(z_t);
    z->data = (int32_t *)malloc(eleBytes);
    memcpy(z->data, z_t.data_ptr(), eleBytes);

    delete [] dim_x;
    delete [] dim_y;

    return (void *)z;
}

void print(void *a)
{
    tensor *x = (tensor *)a;

    int64_t *dim_tmp;

    dim_tmp = new int64_t[x->ndim];
    for (int i = 0; i < x->ndim; i++)
        dim_tmp[i] = (int64_t)(x->dims[i]);
    c10::IntArrayRef x_dim (dim_tmp, x->ndim);
    //delete [] dim_tmp;

    torch::Tensor x_t = torch::from_blob(x->data, x_dim, torch::kInt32);
    delete [] dim_tmp;
    std::cout << x_t << std::endl;
}

/*
#include <iostream>
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

void *mult_int(void *a, void *b, int m, int n, int p)
{
    int i, j, k, numele;
    numele = m * n;
    int *x = (int *)a;
    int *y = (int *)b;
    int *z = (int *)malloc(sizeof(int)*numele);
    memset((void *)z, 0, sizeof(int)*numele);
    for (i = 0; i < m; i++) {
        for (j = 0; j < n; j++)
            for (k = 0; k < p; k++)
                z[i*n+j] += x[i*p+k] * y[k*n+j];
    }
    return (void *)z;
}

void *mult_float(void *a, void *b, int m, int n, int p)
{
    int i, j, k, numele;
    numele = m * n;
    float *x = (float *)a;
    float *y = (float *)b;
    float *z = (float *)malloc(sizeof(float)*numele);
    memset((void *)z, 0, sizeof(float)*numele);
    for (i = 0; i < m; i++) {
        for (j = 0; j < n; j++)
            for (k = 0; k < p; k++)
                z[i*n+j] += x[i*p+k] * y[k*n+j];
    }
    return (void *)z;
}

void *mult(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    if (x->type != y->type)
        die("Not consistent type");
    if (x->ndim != y->ndim)
        die("Not consistent ndim");
    
    int type = (int)x->type;
    int ndim = (int)x->ndim;
    if (ndim != 2)
        die("Not support %d, Only support 2-dim", ndim);
    
    if (x->dims[1] != y->dims[0])
        die("Not consistent dim");
    int8_t *dims = (int8_t *)malloc(sizeof(int8_t)*2);
    
    int m = x->dims[0], p = x->dims[1], n = y->dims[1];
    dims[0] = m; dims[1] = n;

    void *data_z;
    switch (type)
    {
        case 0: data_z = mult_int(x->data, y->data, m, n, p); break;
        case 1: data_z = mult_float(x->data, y->data, m, n, p); break;
    
        default: free(dims); die("Invalid type");
    }

    tensor *z = (tensor *)malloc(sizeof(tensor));
    z->type = (int8_t)type;
    z->ndim = (int8_t)ndim;
    z->dims = dims;
    z->data = data_z;
    return (void *)z;
}

void print_int(void *a, int m, int n)
{
    int *x = (int *)a, i, j;
    for (i = 0; i < m; i++) {
        for (j = 0; j < n; j++)
            if (j == n-1)
                //printf("%d", x[i*n+j]);
                std::cout << x[i*n+j];
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
}*/