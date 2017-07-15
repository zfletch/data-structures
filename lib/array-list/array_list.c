#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "array_list.h"

#define INITIAL_ARRAY_LENGTH 10

typedef struct ArrayList {
	size_t length;
	size_t index;
	int *array;
} ArrayList;

ArrayList *array_list_create()
{
	ArrayList *list = malloc(sizeof(ArrayList));

	list->length = INITIAL_ARRAY_LENGTH;
	list->index = 0;
	list->array = calloc(list->length, sizeof(int));

	return list;
}

bool array_list_empty(ArrayList *list)
{
	return !list->index;
}

int array_list_length(ArrayList *list)
{
	return list->index;
}

int array_list_get(ArrayList *list, size_t index)
{
	if (index >= list->index) return 0;

	return list->array[index];
}

ArrayList *array_list_set(ArrayList *list, size_t index, int value)
{
	if (index >= list->length) {
		size_t length = (index + 1) * 2;
		int *array = calloc(length, sizeof(int));

		memcpy(array, list->array, list->length * sizeof(int));
		free(list->array);

		list->length = length;
		list->array = array;
	}

	if (index >= list->index) list->index = index + 1;

	list->array[index] = value;

	return list;
}

ArrayList *array_list_add(ArrayList *list, int value)
{
	return array_list_set(list, list->index, value);
}
