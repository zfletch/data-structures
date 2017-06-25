#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_ARRAY_LENGTH 10

typedef struct ArrayList {
	size_t length;
	int *array;
} ArrayList;

ArrayList *array_list_create()
{
	ArrayList *list = malloc(sizeof(ArrayList));
	list->length = INITIAL_ARRAY_LENGTH;
	list->array = calloc(list->length, sizeof(int));

	return list;
}

int array_list_get(ArrayList *list, size_t index)
{
	if (index >= list->length) {
		return 0;
	}

	return list->array[index];
}

ArrayList *array_list_set(ArrayList *list, size_t index, int value)
{
	if (index >= list->length) {
		size_t length = (index + 1) * 2;
		int *array = calloc(length, sizeof(int));

		memcpy(list->array, array, list->length * sizeof(int));
		free(list->array);

		list->length = length;
		list->array = array;
	}

	list->array[index] = value;

	return list;
}

/* int main() */
/* { */
/* 	ArrayList *list = array_list_create(); */
/*  */
/* 	array_list_set(list, 140, 12); */
/*  */
/* 	printf("%d %d %d\n", array_list_get(list, 140), array_list_get(list, 10), array_list_get(list, 900)); */
/* } */
