#include <stdio.h>


void printi(void *s)
{
    printf("%s", (char *) s);
}

#ifdef BUILD_TEST
int main()
{
    return 1;
}
#endif