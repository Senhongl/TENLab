#include "tensor.h"
#include <thread>
#include <vector>
#include <future>
#include <iostream>

using namespace std;

typedef void* (*pf)(void*, void*);
typedef void* (*rd)(void**);

extern "C" void *pe_calc(pf* mapfunctions, int num, rd reduce, void* a, void* b)
{
	int i;
	future<void*> pres[num];
	void *res[num] = {0};
	
	for (i = 0;i < num;i++) {
		pres[i] = async(((*mapfunctions[i])), a, b);
	}

	for (i = 0;i < num;i++) {
		res[i] = pres[i].get();
	}

	return (*reduce)(res);
}