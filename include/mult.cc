#include "tensor.h"

/**
 * sample: [[1,2,3],[4,1,0]] * [[1,0,5],[0,2,0]]   
 * output:
 * Not consistent dimension for matrix multiplication
 * 
 * sample: [[1,2,3],[4,1,0]] * [[1,0,5],[0,2,0],[2,3,5]]
 * output:
 * 7  13  20
 * 4   2  20
 * [ CPUIntType{2,3} ]
 */

torch::Tensor mult_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::matmul(x_t, y_t);
    return z_t;
}

extern "C" void *mult(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");
    check(x->ndim <= 2, "Matrix multiplication for dim >= 3 not allowed\n");
    check(y->ndim <= 2, "Matrix multiplication for dim >= 3 not allowedn\n");
    check(x->dims[x->ndim-1] == y->dims[0], "Not consistent dimension for matrix multiplication\n");

    return (void *)fromTensor(mult_t(toTensor(x), toTensor(y)));
}