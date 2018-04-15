#include <stdio.h>
#include <stdlib.h>

//#include <stdbool.h>
#define TRUE    1
#define FALSE   0

#define printf_debug printf

int solution(int bin)
{
    int i = 0;
    while( bin ){
        bin = bin & (bin - 1);
        printf_debug("turn: ");
        print_binary(bin);
        printf_debug("\n");
        i++;
    }

    return i;
}

int print_binary(int bin)
{
    if( !bin ){
        return 0;
    }

    int bit_1 = 0;

    if(bin & 0x8000){
        printf("1");
        bit_1 = 1;
    }
    else{
        printf("0");
    }

    return bit_1 + print_binary(bin << 1);
}

int main(void)
{
    int bin = 0x1359;

    int bit_1;
    printf("bin: ");
    bit_1 = print_binary(bin);
    printf("\n");
    printf("o: bit_1: %d\n", bit_1);

    printf("s: bit_1: %d\n", solution(bin));
}
