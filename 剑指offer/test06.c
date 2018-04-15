#include <stdio.h>
#include <stdlib.h>

//#include <stdbool.h>
#define TRUE    1
#define FALSE   0

#define printf_debug //printf

typedef struct node *p_node;
typedef struct node{
    int value;
    p_node left;
    p_node right;
}node;

p_node solution(int *order_prev, int *order_mid,  int len){
    int i;
    p_node p;

    if( order_prev == NULL || order_mid == NULL || len == 0 ){
        return NULL;
    }
    printf_debug("list len:%d\n", len);

    p = (p_node) malloc(sizeof(node));
    p->value = order_prev[0];
    printf_debug("root:%d\n", p->value);

    for(i = 0; i < len; i++){
        if( order_mid[i] == p->value ){
            break;
        }
    }
    printf_debug("root in mid:%d\n", i);

    p->left = solution(&order_prev[1], &order_mid[0], i);
    p->right = solution(&order_prev[i+1], &order_mid[i+1], len-(i+1));

    return p;
}

void print_order_prev(p_node p)
{
    if( p == NULL ){
        return;
    }

    printf("%d ", p->value);
    print_order_prev(p->left);
    print_order_prev(p->right);
}

void print_order_mid(p_node p)
{
    if( p == NULL ){
        return;
    }

    print_order_mid(p->left);
    printf("%d ", p->value);
    print_order_mid(p->right);
}

void print_order_next(p_node p)
{
    if( p == NULL ){
        return;
    }

    print_order_next(p->left);
    print_order_next(p->right);
    printf("%d ", p->value);
}

int main(void)
{
    #define LISTLEN 8
    int order_prev[LISTLEN] = {1, 2, 4, 7, 3, 5, 6, 8};
    int order_mid[LISTLEN] = {4, 7, 2, 1, 5, 3, 8, 6};

    printf("orde_prev: ");
    for(int i = 0; i < LISTLEN; i++){
        printf("%d ", order_prev[i]);
    }
    printf("\n");
    
    printf("orde_mid: ");
    for(int i = 0; i < LISTLEN; i++){
        printf("%d ", order_mid[i]);
    }
    printf("\n");

    p_node res = solution(&order_prev, &order_mid, LISTLEN);
    
    printf("order_prev: ");
    print_order_prev(res);
    printf("\n");

    printf("order_mid: ");
    print_order_mid(res);
    printf("\n");

    printf("order_next: ");
    print_order_next(res);
    printf("\n");

    return 0;
}
