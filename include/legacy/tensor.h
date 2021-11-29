#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define die(...) \
do {\
    fprintf(stderr, __VA_ARGS__);\
    perror(" ");\
    exit(1);\
} while(0)

typedef struct tensor
{
    int8_t type;
    int8_t ndim;
    int8_t *dims;
    void *data;
} tensor;

void *add(void *, void *);
void *mult(void *, void *);

void print(void *);