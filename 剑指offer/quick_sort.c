#include <stdio.h>
#include <stdlib.h>

//#include <stdbool.h>
#define TRUE    1
#define FALSE   0

#define printf_debug printf

int partition(int *arr, int len)
{
    if( arr == NULL || len < 2 ){
        return 0;
    }

    int target = arr[len-1];
    printf_debug("target: %d\n", target);

    int *small_p = arr;
    for(int i = 0; i < len - 1; i++){
        if( arr[i] < target ){
            int tmp = arr[i];
            arr[i] = *small_p;
            *small_p = tmp;
            small_p++;
        }
    }

    arr[len-1] = *small_p;
    *small_p = target;

    printf_debug("after partition: ");
    for(int i = 0; i < len; i++){
        printf_debug("%d ", arr[i]);
    }
    printf_debug("\n");

    return (small_p - arr);
}

void quick_sort(int *arr, int len)
{
    if( arr == NULL || len < 2 ){
        return;
    }

    int mid = partition(arr, len);
    printf_debug("len: %d, mid: %d\n", len, mid);

    quick_sort(&arr[0], mid);
    quick_sort(&arr[mid+1], len-mid);

    printf_debug("after quick_sort: ");
    for(int i = 0; i < len; i++){
        printf_debug("%d ", arr[i]);
    }
    printf_debug("\n");
}


int main(void)
{
    #define LISTLEN 9
    int arr[LISTLEN] = {4, 7, 2, 1, 5, 3, 8, 6, 0};    

    printf("arr: ");
    for(int i = 0; i < LISTLEN; i++){
        printf("%d ", arr[i]);
    }
    printf("\n");

    quick_sort(&arr, LISTLEN);
}
