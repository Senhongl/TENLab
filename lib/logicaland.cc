#include "tensor.h"

/**
 * sample: [[1,2,3],[4,1,0]] && [[1,0,5],[0,2,0]]  
 * output:
 * 1  0  1
 * 0  1  0
 * [ CPUIntType{2,3} ]
 * 
 * sample: [1] && [0]  
 *  0
 * [ CPUIntType{1} ]
 */

torch::Tensor logicaland_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::logical_and(x_t,y_t);
    return z_t.toType(torch::kInt32);
}

extern "C" void *logicaland(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");
    check(x->ndim == y->ndim, "Not consistent dimension\n");
    bool flag = true;
    for (int i = 0; i < x->ndim; i++) {
        if (x->dims[i] != y->dims[i]) {
            flag = false;
            break;
        }
    }
    check(flag == true, "Not consistent dimension\n");

    return (void *)fromTensor(logicaland_t(toTensor(x), toTensor(y)));
}