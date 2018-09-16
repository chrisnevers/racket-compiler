#include <stdio.h>

void print_int(int i)
{
    printf("%d\n", i);
}

void print_bool(int i)
{
    if (i) {
        printf("#t\n");
    } else {
        printf("#f\n");
    }
}

void print_unit(int i)
{
    printf("()\n");
}

int read_int()
{
    int i;
    scanf("%d", &i);
    return i;
}
