#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <stdlib.h>

typedef void* (*pf)(void*, void*);
typedef void* (*rd)(void**);

void *pe_calc(pf* mapfunctions, int num, rd reduce, void* a, void* b)
{
	int i;
	pid_t pid;
	void** res = mmap(NULL, num*sizeof(void*), PROT_READ | PROT_WRITE,
                      MAP_SHARED | MAP_ANON, -1, 0);
	for (i = 0;i < num;i++) {
		pid = fork();
		if (pid == 0) {
			res[i] = (*(mapfunctions[i]))(a,b);
			exit(0);
		}	
	}
	void* sol = reduce(res);
	munmap(res, num*sizeof(void*));
	return sol;
}