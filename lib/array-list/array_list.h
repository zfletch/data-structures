#ifndef ARRAY_LIST_H
#define ARRAY_LIST_H

typedef struct ArrayList ArrayList;
ArrayList *array_list_create();
void array_list_destroy(ArrayList *list);
bool array_list_is_empty(ArrayList *list);
int array_list_length(ArrayList *list);
int array_list_get(ArrayList *list, size_t index);
ArrayList *array_list_set(ArrayList *list, size_t index, int value);
ArrayList *array_list_push(ArrayList *list, int value);

#endif
