#include <stdio.h>
//#include <stdbool.h>
#define TRUE    1
#define FALSE   0

#define printf_debug //printf

char *solution(const char *str, char target_c, const char *replace_c)
{
    if( str == NULL || replace_c == NULL ){
        return NULL;
    }

    printf("%s\n", str);

    int rep_len = 0;
    for(char *r = replace_c; *r != '\0'; r++){
        rep_len++;
    }
    printf_debug("rep len:%d\n", rep_len);

    int space_add = rep_len - 1;

    int target_c_num = 0, str_len = 0;
    for(char *s = str; *s != '\0'; s++){
        if( *s == target_c ){
            target_c_num++;
        }
        str_len++;
    }
    printf_debug("str len: %d, target char num:%d\n", str_len, target_c_num);

    int res_len = str_len + target_c_num*space_add + 1;
    char *res = (char*) malloc(sizeof(char)*res_len);

    char *res_p = res;
    for(int i=0; i<str_len; i++){
        if( str[i] == target_c ){
            for(char *p = replace_c; *p != '\0'; p++){
                *res_p = *p;
                res_p++;
            }
        }
        else{
            *res_p = str[i];
            res_p++;
        }
    }

    *res_p = '\0';

    printf("%s\n", res);
}


int main(void)
{
    char *str = "We are happy.";
    char target_c = ' ';
    char *replace_c = "%20";

    char *res;

    res = solution(str, target_c, replace_c);

}
