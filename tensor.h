#include <stdio.h>
#include <stdlib.h>

#define die(...) \
do {\
    fprintf(stderr, __VA_ARGS__);\
    perror(" ");\
    exit(1);\
} while(0)

typedef struct tensor
{
    char type;
    char ndim;
    char *dims;
    char *data;
} tensor;