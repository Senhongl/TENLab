#include <thread>
#include <vector>
#include <future>
#include <iostream>
#include <cstdint>

using namespace std;

typedef void* (*pf)(void*, void*);
typedef void* (*rd)(void**);

void* pl(void* a, void* b) {
	int c = reinterpret_cast<intptr_t>(a);
	int d = reinterpret_cast<intptr_t>(b);
	return reinterpret_cast<void*>(c+d);
}

extern "C" void *pe_calc(pf* mapfunctions, int num, rd reduce, void* a, void* b)
{
	int i;
	future<void*> pres[num];
	void *res[num] = {0};

	cout<<"before"<<endl;
	
	for (i = 0;i < num;i++) {
		cout<<i<<endl;
		pres[i] = async(pl, reinterpret_cast<void*>(1), reinterpret_cast<void*>(2));
	}

	cout<<"asynced"<<endl;

	for (i = 0;i < num;i++) {
		res[i] = pres[i].get();
	}

	cout<<"end"<<endl;

	// return (*reduce)(res);
	return res[0];
}

int main() {
	cout << pe_calc(NULL, 2, NULL, NULL, NULL) << endl;
	return 0;
}