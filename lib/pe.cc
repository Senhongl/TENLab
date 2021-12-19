#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <stdlib.h>
#include "tensor.h"

typedef void* (*pf)(void*, void*);
typedef void* (*rd)(void**);

extern "C" void *pe_calc(pf* mapfunctions, int num, rd reduce, void* a, void* b)
{
	int i;
	pid_t pid;
	void** res = reinterpret_cast<void**>(mmap(NULL, num*sizeof(void*), PROT_READ | PROT_WRITE,
                      MAP_SHARED | MAP_ANON, -1, 0));
	res[0] = (*(mapfunctions[0]))(a,b);
	res[1] = (*(mapfunctions[1]))(a,b);
	void* sol = (*reduce)(res);
	munmap(reinterpret_cast<void*>(res), num*sizeof(void*));
	return sol;
}