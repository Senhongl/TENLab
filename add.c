#include "tensor.h"

void *add(void *a, void *b)
{
    /*tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;*/

    /*if (x->type != y->type)
        die("Not consistent type");
    if (x->ndim != y->ndim)
        die("Not consistent ndim");
    
    int type = (int)x->type;
    printf("Type %d\n", type);
    int ndim = (int)x->ndim, i;
    printf("Dim %d: ", ndim);
    for (i = 0; i < ndim; i++) {
        if (x->dims[i] != y->dims[i])
            die("Not consistent dim %d", i);
        printf("%d, ", x->dims[i]);
    }
    printf("\n");*/
}

#ifdef BUILD_TEST
int main()
{
    return 1;
}
#endif