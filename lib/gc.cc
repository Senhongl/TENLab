#include "tensor.h"
#include <stdlib.h>

void free_tensor(tensor *a)
{   
    tensor *x = a;
    if (x -> rc < 0) {
        printf("bug comming :)!\n");
    } else if (x -> rc > 0) {
        return;
    }

    free(x);
}

extern "C" void increase_rc(void *a)
{
    tensor *x = (tensor *)a;
    if (x -> type != 3) {
        x -> rc += 1;
        return;
    }

    x -> rc += 1;
    int8_t dim = x->dims[0], i;
    tensor **data = (tensor **)x->data;
    for (i = 0; i < dim; i++) {
        increase_rc(data[i]);
    }
}

extern "C" void decrease_rc(void *a)
{
    if (a == NULL) return;
    tensor *x = (tensor *)a;
    if (x -> type != 3) {
        x -> rc -= 1;
        if (x -> rc == 0) {
            free_tensor(x);
        }
        return;
    }

    x -> rc -= 1;
    int8_t dim = x->dims[0], i;
    tensor **data = (tensor **)x->data;
    for (i = 0; i < dim; i++) {
        decrease_rc(data[i]);
    }
    
    if (x -> rc == 0) {
        free_tensor(x);
    }
}