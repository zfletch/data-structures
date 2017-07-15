#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct Heap {
	int *array;
	size_t index;
	size_t size;
} Heap;

Heap *heap_create(size_t size)
{
	Heap *heap = malloc(sizeof(Heap));

	heap->index = 0;
	heap->size = size;
	heap->array = malloc(size * sizeof(int));

	return heap;
}

bool heap_empty(Heap *heap)
{
	return !heap->index;
}

bool heap_full(Heap *heap)
{
	return heap->index == heap->size;
}

bool heap_insert(Heap *heap, int val)
{
	if (heap_full(heap)) return false;

	size_t insert_index = heap->index++;
	size_t parent_index = (insert_index - 1) / 2;

	heap->array[insert_index] = val;

	while (insert_index > 0 && heap->array[insert_index] < heap->array[parent_index]) {
		int temp = heap->array[parent_index];
		heap->array[parent_index] = heap->array[insert_index];
		heap->array[insert_index] = temp;

		insert_index = parent_index;
		parent_index = (insert_index - 1) / 2;
	}

	return true;
}

int heap_find_min(Heap *heap)
{
	if (heap_empty(heap)) return 0;

	return heap->array[0];
}

int heap_delete_min(Heap *heap)
{
	if (heap_empty(heap)) return 0;

	int min = heap->array[0];

	heap->array[0] = heap->array[--heap->index];

	size_t index = heap->index;
	size_t delete_index = 0;
	size_t left_index = delete_index * 2 + 1;
	size_t right_index = left_index + 1;

	while (left_index < index) {
		if (left_index == index - 1) {
			if (heap->array[left_index] < heap->array[delete_index]) {
				int temp = heap->array[delete_index];
				heap->array[delete_index] = heap->array[left_index];
				heap->array[left_index] = temp;
			}

			break;
		} else {
			if (heap->array[left_index] < heap->array[delete_index]
					&& heap->array[left_index] <= heap->array[right_index]) {
				int temp = heap->array[delete_index];
				heap->array[delete_index] = heap->array[left_index];
				heap->array[left_index] = temp;

				delete_index = left_index;
			} else if (heap->array[right_index] < heap->array[delete_index]
					&& heap->array[right_index] <= heap->array[left_index]) {
				int temp = heap->array[delete_index];
				heap->array[delete_index] = heap->array[right_index];
				heap->array[right_index] = temp;

				delete_index = right_index;
			} else {
				break;
			}
		}

		left_index = delete_index * 2 + 1;
		right_index = left_index + 1;
	}

	return min;
}

/* int main() */
/* { */
/* 	Heap *heap = heap_create(20); */
/*  */
/* 	heap_insert(heap, 5); */
/* 	heap_insert(heap, 4); */
/* 	heap_insert(heap, 12); */
/* 	heap_insert(heap, 3); */
/* 	heap_insert(heap, 8); */
/*  */
/* 	size_t index = heap->index; */
/*  */
/* 	for (int i = 0; i < index; i++) { */
/* 		printf("%d\n", heap->array[i]); */
/* 	} */
/*  */
/* 	printf("\n"); */
/*  */
/* 	for (int i = 0; i < index; i++) { */
/* 		printf("%d\n", heap_delete_min(heap)); */
/* 	} */
/* } */
