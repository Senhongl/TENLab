#include "tensor.h"

extern "C" void print(void *a)
{
    tensor *x = (tensor *)a;

    std::cout << toTensor(x) << std::endl;
}