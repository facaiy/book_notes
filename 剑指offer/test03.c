#include <stdio.h>
//#include <stdbool.h>
#define TRUE    1
#define FALSE   0

#define printf_debug printf

int main(void)
{
    #define ROWS    4 
    #define COLUMNS 4
    int matrix[ROWS][COLUMNS] = {  
        {1, 2, 8, 9},  
        {2, 4, 9, 12},  
        {4, 7, 10, 13},  
        {6, 8, 11, 15}  
    };  

    if( exam_03(matrix, ROWS, COLUMNS, 7) )
        printf("FOUND\n");
    else
        printf("NONE\n");
}


int exam_03(int* matrix, int rows, int columns,  int number)
{

    if( (matrix == NULL) || (rows < 1) || (columns < 1) ){
        return FALSE;
    }

    int r =  0;
    int c = columns -1;
    printf_debug("r:%d, c:%d\n", r, c);

    while( (r < rows) && (c >= 0) ){
        int find_value = matrix[r*columns+c];

        printf_debug("target:%d, now:%d\n", number, find_value);
        if( number < find_value ){
            c--;
        }
        else if( number > find_value ){
            r++;
        }
        else{
            return TRUE;
        }

        printf_debug("r:%d, c:%d\n", r, c);
    }

    return FALSE;
}
