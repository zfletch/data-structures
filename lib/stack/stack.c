#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct Stack {
	int index;
	int size;
	int *values;
} Stack;

Stack *stack_create(int size)
{
	Stack *stack = malloc(sizeof(Stack));
	int *values = malloc(size * sizeof(int));

	stack->index = -1;
	stack->size = size;
	stack->values = values;

	return stack;
}

void stack_destroy(Stack *stack)
{
	free(stack->values);
	free(stack);
}

bool stack_push(Stack *stack, int value)
{
	if (stack->index >= stack->size - 1) {
		return false;
	}

	stack->values[++stack->index] = value;
	return true;
}

bool stack_is_empty(Stack *stack)
{
	if (stack->index < 0) {
		return true;
	}

	return false;
}

int stack_pop(Stack *stack)
{
	if (stack_is_empty(stack)) {
		return 0;
	}

	return stack->values[stack->index--];
}

/* int main() */
/* { */
/* 	Stack *stack = stack_create(3); */
/* 	printf("%d\n", stack_push(stack, 10)); // => 1 */
/* 	printf("%d\n", stack_push(stack, 11)); // => 1 */
/* 	printf("%d\n", stack_push(stack, 12)); // => 1 */
/* 	printf("%d\n", stack_push(stack, 13)); // => 0 */
/*  */
/* 	printf("%d\n", stack_is_empty(stack)); // => 0 */
/*  */
/* 	printf("%d\n", stack_pop(stack)); // => 12 */
/* 	printf("%d\n", stack_pop(stack)); // => 11 */
/* 	printf("%d\n", stack_pop(stack)); // => 10 */
/* 	printf("%d\n", stack_pop(stack)); // => 0 */
/* 	printf("%d\n", stack_is_empty(stack)); // => 1 */
/*  */
/* 	stack_destroy(stack); */
/* } */
