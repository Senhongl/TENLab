#include <pthread.h>

void *init(void* ignore)
{
	ignore = NULL;
	pthread_exit(NULL);
}

int main()
{
	pthread_t *thread1 = NULL, *thread2 = NULL;
	pthread_create(thread1, NULL, init, NULL);
	pthread_create(thread2, NULL, init, NULL);
	pthread_join(*thread1, NULL);
	pthread_join(*thread2, NULL);
	return 0;
}
