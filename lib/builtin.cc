#include "tensor.h"
#include <array>

torch::Tensor add_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t + y_t;
    return z_t;
}

extern "C" void *add(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");
    if (y->ndim != 0) {
        check(x->ndim == y->ndim, "Not consistent dimension\n");
        bool flag = true;
        for (int i = 0; i < x->ndim; i++) {
            if (x->dims[i] != y->dims[i]) {
                flag = false;
                break;
            }
        }
        check(flag == true, "Not consistent dimension\n");
    }


    return (void *)fromTensor(add_t(toTensor(x), toTensor(y)));
}

torch::Tensor all_t(const torch::Tensor &x_t)
{
    torch::Tensor a_t = torch::all(x_t);
    return a_t.toType(torch::kInt32);
}

extern "C" void *all(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(all_t(toTensor(x)));
}

torch::Tensor any_t(const torch::Tensor &x_t)
{
    torch::Tensor a_t = torch::any(x_t);
    return a_t.toType(torch::kInt32);
}

extern "C" void *any(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(any_t(toTensor(x)));
}

extern "C" bool bool_of_zero(void *a)
{
    tensor *x = (tensor *)a;
    if (*(int *)(x -> data) == 0) {
        return false;
    } else {
        return true;
    }
}

torch::Tensor cat_t(const torch::Tensor &x_t, const torch::Tensor &y_t, const torch::Tensor &dim_t)
{
    torch::Tensor a_t = torch::cat({x_t, y_t}, dim_t.item().to<int64_t>());
    return a_t;
}

extern "C" void *cat(void *a, void *b, void *c)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;
    tensor *z = (tensor *)c;

    check(x->type == y->type, "Not consistent type");
    check(z->ndim == 0, "Second tensor should be 0-dim tensor");

    return (void *)fromTensor(cat_t(toTensor(x), toTensor(y), toTensor(z)));
}

torch::Tensor divide_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t.div(y_t.item());
    return z_t;
}

extern "C" void *divide(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor");

    return (void *)fromTensor(divide_t(toTensor(x), toTensor(y)));
}

torch::Tensor dotmul_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t * y_t;
    return z_t;
}

extern "C" void *dotmul(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");
    if (y->ndim != 0) {
        check(x->ndim == y->ndim, "Not consistent dimension\n");
        bool flag = true;
        for (int i = 0; i < x->ndim; i++) {
            if (x->dims[i] != y->dims[i]) {
                flag = false;
                break;
            }
        }
        check(flag == true, "Not consistent dimension\n");
    }

    return (void *)fromTensor(dotmul_t(toTensor(x), toTensor(y)));
}


torch::Tensor dotpow_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    int64_t exp = y_t.item().to<int64_t>();
    torch::Tensor z_t = x_t;
    for (int i = 1; i < exp; i++)
        z_t = z_t * x_t;
    return z_t;
}

extern "C" void *dotpow(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(y->type == 0, "Second tensor should have int type\n");
    check(y->ndim == 0, "Second tensor should be 0-dim\n");

    return (void *)fromTensor(dotpow_t(toTensor(x), toTensor(y)));
}

torch::Tensor equal_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t == y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *equal(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    //check(x->type == y->type, "Not consistent type");
    if (x->type != y->type) {
        tensor *ret = (tensor *)malloc(sizeof(tensor));
        ret->type = 0;
        ret->ndim = 0;
        ret->dims = NULL;
        ret->data = malloc(sizeof(int));
        *(int *)(ret->data) = 0;
        return (void *)ret;
    }
    if (x->type == 21) {
        tensor *ret = (tensor *)malloc(sizeof(tensor));
        ret->type = 0;
        ret->ndim = 0;
        ret->dims = NULL;
        ret->data = malloc(sizeof(int));
        *(int *)(ret->data) = 1;
        return (void *)ret;
    }

    return (void *)fromTensor(equal_t(toTensor(x), toTensor(y)));
}


torch::Tensor floordivide_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    std::string rounding_mode = "floor";
    torch::Tensor z_t = torch::div(x_t, y_t.item(), rounding_mode);
    return z_t;
}

extern "C" void *floordivide(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor\n");

    return (void *)fromTensor(floordivide_t(toTensor(x), toTensor(y)));
}

torch::Tensor greater_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t > y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *greater(void *a, void *b)
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

    return (void *)fromTensor(greater_t(toTensor(x), toTensor(y)));
}

torch::Tensor greaterequal_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t >= y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *greaterequal(void *a, void *b)
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

    return (void *)fromTensor(greaterequal_t(toTensor(x), toTensor(y)));
}

torch::Tensor less_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t < y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *less(void *a, void *b)
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

    return (void *)fromTensor(less_t(toTensor(x), toTensor(y)));
}

torch::Tensor lessequal_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t <= y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *lessequal(void *a, void *b)
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

    return (void *)fromTensor(lessequal_t(toTensor(x), toTensor(y)));
}

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

torch::Tensor logicalnot_t(const torch::Tensor &x_t)
{
    torch::Tensor z_t = torch::logical_not(x_t);
    return z_t.toType(torch::kInt32);
}

extern "C" void *logicalnot(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(logicalnot_t(toTensor(x)));
}

torch::Tensor logicalor_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::logical_or(x_t,y_t);
    return z_t.toType(torch::kInt32);
}

extern "C" void *logicalor(void *a, void *b)
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

    return (void *)fromTensor(logicalor_t(toTensor(x), toTensor(y)));
}

torch::Tensor matpow_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::linalg::matrix_power(x_t, y_t.item().to<int64_t>());
    return z_t;
}

extern "C" void *matpow(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(y->type == 0, "Second tensor should have int type\n");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor\n");

    return (void *)fromTensor(matpow_t(toTensor(x), toTensor(y)));
}

torch::Tensor mod_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = torch::fmod(x_t, y_t.item());
    return z_t;
}

extern "C" void *mod(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Second tensor should have int type\n");
    check(y->ndim == 0, "Second tensor should be 0-dim tensor\n");

    return (void *)fromTensor(mod_t(toTensor(x), toTensor(y)));
}

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

torch::Tensor notequal_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t != y_t;
    return z_t.toType(torch::kInt32);
}

extern "C" void *notequal(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    //check(x->type == y->type, "Not consistent type\n");
    if (x->type != y->type) {
        tensor *ret = (tensor *)malloc(sizeof(tensor));
        ret->type = 0;
        ret->ndim = 0;
        ret->dims = NULL;
        ret->data = malloc(sizeof(int));
        *(int *)(ret->data) = 1;
        return (void *)ret;
    }
    if (x->type == 21) {
        tensor *ret = (tensor *)malloc(sizeof(tensor));
        ret->type = 0;
        ret->ndim = 0;
        ret->dims = NULL;
        ret->data = malloc(sizeof(int));
        *(int *)(ret->data) = 0;
        return (void *)ret;
    }
    check(x->ndim == y->ndim, "Not consistent dimension\n");
    bool flag = true;
    for (int i = 0; i < x->ndim; i++) {
        if (x->dims[i] != y->dims[i]) {
            flag = false;
            break;
        }
    }
    check(flag == true, "Not consistent dimension\n");

    return (void *)fromTensor(notequal_t(toTensor(x), toTensor(y)));
}

void print_var(tensor *x)
{
    if (x->type != 3) {
        if (x->type == 21)
            printf("Nil\n");
        else if (x->type == 2)
            printf("%s\n", (char *)x->data);
        else
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
    } else if (x->type == 2)
        printf("%s\n", (char *)x->data);
    else if (x->type == 21)
        printf("Nil\n");
    else
        std::cout << toTensor(x) << std::endl;
}

void PrintStuff( char to_print, int length ) {

    // adjust buffer size as desired
    char buffer[256];

    // -1 for null terminator
    if( length > sizeof(buffer)-1 ) length = sizeof(buffer)-1; 

    // fill buffer with desired character
    memset( buffer, to_print, length );

    // add null terminator
    buffer[length] = 0;

    // print to output
    printf("%s", buffer);
}

extern "C" void print_error(void *a, void *b)
{
    tensor *epoch = (tensor *)a;
    int e = toTensor(epoch).item().to<int>();
    printf("Training Epoch: %d    ", e);
    printf("[");
    PrintStuff('=', e * 2 + 1);
    printf("]");
    printf("\n");
    printf("Mean Square Error: ");
    tensor *x = (tensor *)b;
    std::cout << toTensor(x).item().to<float>() << std::endl;
}

extern "C" void print_int(int a)
{
    printf("%d\n", a);
}

extern "C" void *range(void *a, void *b, void *c)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;
    tensor *z = (tensor *)c;

    check(x->type == 0 && y->type == 0 && z->type == 0, "Slice must be integer");
    check(x->ndim == 0 && y->ndim == 0 && z->ndim == 0, "Slice must be integer");

    int indx = *(int *)x->data;
    int indy = *(int *)y->data;
    int indz = *(int *)z->data;

    tensor *n = fromTensor(torch::arange(indx, indy, indz).to(torch::kInt32));
    int len = n->dims[0];
    free(n->dims);
    n->dims = (int64_t *)malloc(sizeof(int64_t) * (n->ndim + 3));
    n->dims[0] = len;
    n->dims[1] = indx;
    n->dims[2] = indy;
    n->dims[3] = indz;
    n->type = 4;
    return (void *)n;
}

torch::Tensor shape_t(const torch::Tensor &x_t)
{
    c10::IntArrayRef a = x_t.sizes();
    int64_t size = a.size();
    int64_t dim_tmp[size];
    for (int i = 0; i < size; i++){
        dim_tmp[i] = (int64_t)(a[i]);
    }
    c10::IntArrayRef a_dim (size);
    auto options = torch::TensorOptions().dtype(torch::kInt64);
    torch::Tensor z_t = torch::from_blob(dim_tmp, a_dim, options);
    return z_t.toType(torch::kInt32).clone();
}

extern "C" void *shape(void *a)
{
    tensor *x = (tensor *)a;
    if (x->type == 3) {
        tensor *ret = (tensor *)malloc(sizeof(tensor));
        ret->type = 0;
        ret->ndim = 0;
        ret->dims = NULL;
        ret->data = malloc(sizeof(int));
        *(int *)(ret->data) = (int)(x->dims[0]);
        return (void *)ret;
    }

    return (void *)fromTensor(shape_t(toTensor(x)));
}


torch::Tensor subtract_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    torch::Tensor z_t = x_t - y_t;
    return z_t;
}

torch::Tensor negative_t(const torch::Tensor &x_t)
{
    return torch::neg(x_t);
}

extern "C" void *subtract(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");
    if (y->ndim != 0) {
        check(x->ndim == y->ndim, "Not consistent dimension\n");
        bool flag = true;
        for (int i = 0; i < x->ndim; i++) {
            if (x->dims[i] != y->dims[i]) {
                flag = false;
                break;
            }
        }
        check(flag == true, "Not consistent dimension\n");
    }


    return (void *)fromTensor(subtract_t(toTensor(x), toTensor(y)));
}

extern "C" void *negative(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(negative_t(toTensor(x)));
}


torch::Tensor transpose_t(const torch::Tensor &x_t)
{
    torch::Tensor z_t = x_t.transpose(-1, 0).contiguous();
    return z_t;
}

extern "C" void *transpose(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(transpose_t(toTensor(x)));
}

torch::Tensor zeros_t(const torch::Tensor &x_t)
{
    int64_t size = x_t.sizes()[0];
    std::vector<int64_t> dims;
    for (int i = 0; i < size; i++) 
        dims.push_back(x_t[i].item().to<int64_t>());
    at::IntArrayRef ndims (dims);
    torch::Tensor z_t = torch::zeros(ndims);
    return z_t.toType(torch::kInt32);
}

extern "C" void *zeros(void *a)
{
    tensor *x = (tensor *)a;
    check(x->type == 0, "Not consistent type");
    check(x->ndim == 1 , "Dimension should be 1");
    return (void *)fromTensor(zeros_t(toTensor(x)));
}

torch::Tensor sum_t(const torch::Tensor &x_t)
{
    torch::Tensor z_t = torch::sum(x_t);
    return z_t;
}

extern "C" void *sum(void *a)
{
    tensor *x = (tensor *)a;
    if (x->type == 0)
        return (void *)fromTensor(sum_t(toTensor(x)).toType(torch::kInt32));
    else
        return (void *)fromTensor(sum_t(toTensor(x)).toType(torch::kFloat64));
}

torch::Tensor ones_t(const torch::Tensor &x_t)
{
    int64_t size = x_t.sizes()[0];
    std::vector<int64_t> dims;
    for (int i = 0; i < size; i++) 
        dims.push_back(x_t[i].item().to<int64_t>());
    at::IntArrayRef ndims (dims);
    torch::Tensor z_t = torch::ones(ndims);
    return z_t.toType(torch::kInt32);
}

extern "C" void *ones(void *a)
{
    tensor *x = (tensor *)a;
    check(x->type == 0, "Not consistent type");
    check(x->ndim == 1 , "Dimension should be 1");
    return (void *)fromTensor(ones_t(toTensor(x)));
}

torch::Tensor tensor_floor_t(const torch::Tensor &x_t)
{
    return torch::floor(x_t.toType(torch::kFloat64));
}


extern "C" void *tensor_floor(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(tensor_floor_t(toTensor(x)));
}

torch::Tensor tensor_ceil_t(const torch::Tensor &x_t)
{
    return torch::ceil(x_t.toType(torch::kFloat64));
}

extern "C" void *tensor_ceil(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(tensor_ceil_t(toTensor(x)));
}

torch::Tensor tensor_round_t(const torch::Tensor &x_t)
{
    return torch::round(x_t.toType(torch::kFloat64));
}

extern "C" void *tensor_round(void *a)
{
    tensor *x = (tensor *)a;
    if (x->type == 0)
        return a;
    return (void *)fromTensor(tensor_round_t(toTensor(x)));
}

torch::Tensor tensor_abs_t(const torch::Tensor &x_t)
{
    return torch::abs(x_t);
}

extern "C" void *tensor_abs(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(tensor_abs_t(toTensor(x)));
}

torch::Tensor tensor_log_t(const torch::Tensor &x_t)
{
    return torch::log(x_t.toType(torch::kFloat64));
}

extern "C" void *tensor_log(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(tensor_log_t(toTensor(x)));
}

torch::Tensor inverse_t(const torch::Tensor &x_t)
{
    return torch::inverse(x_t.toType(torch::kFloat64));
}

extern "C" void *inverse(void *a)
{
    tensor *x = (tensor *)a;
    check(x->ndim == 2, "Only 2-d square matrix allowed for inverse operation\n");
    check(x->dims[0] == x->dims[1], "Only 2-d square matrix allowed for inverse operation\n");
    
    return (void *)fromTensor(inverse_t(toTensor(x)));
}

torch::Tensor int_of_t(const torch::Tensor &x_t)
{
    return x_t.toType(torch::kInt32);
}

extern "C" void *int_of(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(int_of_t(toTensor(x)));
}

torch::Tensor float_of_t(const torch::Tensor &x_t)
{
    return x_t.toType(torch::kFloat64);
}

extern "C" void *float_of(void *a)
{
    tensor *x = (tensor *)a;

    return (void *)fromTensor(float_of_t(toTensor(x)));
}

torch::Tensor solve_t(const torch::Tensor &x_t, const torch::Tensor &y_t)
{
    auto z_t = torch::solve(x_t,y_t);
    return std::get<0>(z_t);  // tbm
}

extern "C" void *solve(void *a, void *b)
{
    tensor *x = (tensor *)a;
    tensor *y = (tensor *)b;

    check(x->type == y->type, "Not consistent type\n");

    return (void *)fromTensor(subtract_t(toTensor(x), toTensor(y)));
}

torch::Tensor tensor_rand_t(const torch::Tensor &x_t)
{
    int64_t size = x_t.sizes()[0];
    std::vector<int64_t> dims;
    for (int i = 0; i < size; i++) 
        dims.push_back(x_t[i].item().to<int64_t>());
    at::IntArrayRef ndims (dims);
    torch::Tensor z_t = torch::rand(ndims);
    return z_t.toType(torch::kFloat64);
}

extern "C" void *tensor_rand(void *a)
{
    tensor *x = (tensor *)a;
    check(x->type == 0, "Not consistent type");
    check(x->ndim == 1 , "Dimension should be 1");
    return (void *)fromTensor(tensor_rand_t(toTensor(x)));
}
