#include "tensor.h"
struct hello {
    int m;
    long n;
};

void mm()
{
    int c[3] = {1,2,3};
}

int main()
{
    //struct hello a = {.m=2, .n=4};
    int c[3] = {1, 2, 3};
    int *d = c;
    mm();
    printf("Hello\n");
    //void *d = (void *)c + 1;
    //int *a = (int *)1;
    //int a = 10;
    //float c = (float)a;
    //int b = c[1];
    //void *d = (void *)&a;
    //void *b;
    //b = (void *)&a;
    return 0;
}