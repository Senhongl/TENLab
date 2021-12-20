#include "tensor.h"

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
        printf("\n");
    }
    else
        std::cout << toTensor(x) << std::endl;
}