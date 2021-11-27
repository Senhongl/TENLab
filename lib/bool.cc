#include "tensor.h"


extern "C" bool bool_of_zero(void *a)
{
    tensor *x = (tensor *)a;
    if (*(int *)(x -> data) == 0) {
        return false;
    } else {
        return true;
    }
}