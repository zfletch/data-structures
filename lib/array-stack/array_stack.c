#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "array_stack.h"

typedef struct ArrayStack {
	size_t index;
	size_t size;
	int *values;
} ArrayStack;

ArrayStack *array_stack_create(size_t size)
{
	ArrayStack *stack = malloc(sizeof(ArrayStack));
	int *values = malloc(size * sizeof(int));

	stack->index = 0;
	stack->size = size;
	stack->values = values;

	return stack;
}

void array_stack_destroy(ArrayStack *stack)
{
	free(stack->values);
	free(stack);
}

bool array_stack_is_empty(ArrayStack *stack)
{
	return !stack->index;
}

bool array_stack_is_full(ArrayStack *stack)
{
	return stack->index == stack->size;
}

bool array_stack_push(ArrayStack *stack, int value)
{
	if (array_stack_is_full(stack)) return false;

	stack->values[stack->index++] = value;
	return true;
}

int array_stack_pop(ArrayStack *stack)
{
	if (array_stack_is_empty(stack)) return 0;

	return stack->values[--stack->index];
}
