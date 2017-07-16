#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct Stack {
	size_t index;
	size_t size;
	int *values;
} Stack;

Stack *stack_create(size_t size)
{
	Stack *stack = malloc(sizeof(Stack));
	int *values = malloc(size * sizeof(int));

	stack->index = 0;
	stack->size = size;
	stack->values = values;

	return stack;
}

void stack_destroy(Stack *stack)
{
	free(stack->values);
	free(stack);
}

bool stack_is_empty(Stack *stack)
{
	return !stack->index;
}

bool stack_push(Stack *stack, int value)
{
	if (stack->index == stack->size) return false;

	stack->values[stack->index++] = value;
	return true;
}

int stack_pop(Stack *stack)
{
	if (stack_is_empty(stack)) return 0;

	return stack->values[--stack->index];
}
