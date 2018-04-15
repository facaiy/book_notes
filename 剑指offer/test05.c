#include <stdio.h>
#include <stdlib.h>

//#include <stdbool.h>
#define TRUE    1
#define FALSE   0

#define printf_debug printf

typedef struct node *p_node;
typedef struct node{
    int value;
    p_node next;
}node;

void solution(p_node list_head)
{
    if(list_head == NULL || list_head->next == NULL)
        return;

    solution(list_head->next);

    printf("%d ", list_head->value);
}


int main(void)
{
    node e = {0, NULL};
    p_node list_head = &e;
    p_node p;
    int i;

    srand(time(NULL));

    for(p = list_head, i = 0; i < 10; i++){
        if(p->next == NULL){
            p->next = (p_node) malloc(sizeof(node));
            p->next->next = NULL;
        }

        p->value = rand() % 50;
        p = p->next;
    }

    for(p = list_head; p->next != NULL; p = p->next){
        printf("%d ", p->value);
    }
    printf("\n");

    solution(list_head);
    printf("\n");

    return 0;
}
