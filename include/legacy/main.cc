#include "tensor.h"
#include <iostream>

int main() {
    int32_t data[4] = {1,2,3,4};
    int8_t dim[2] = {2,2};
    int8_t ndim = 2;
    int8_t type = 0;
    tensor t = {type, ndim, dim, (void *)data};
    print((void *)&t);
}