#ifndef ARRAY_LIST_H
#define ARRAY_LIST_H

typedef struct ArrayList ArrayList;
ArrayList *array_list_create();
bool array_list_empty(ArrayList *list);
int array_list_length(ArrayList *list);
int array_list_get(ArrayList *list, size_t index);
ArrayList *array_list_set(ArrayList *list, size_t index, int value);
ArrayList *array_list_add(ArrayList *list, int value);

#endif
