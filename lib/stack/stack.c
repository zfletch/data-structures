#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "stack.h"

typedef struct Node {
	struct Node *next;
	int value;
} Node;

typedef struct Stack {
	Node *head;
} Stack;

Stack *stack_create()
{
	Stack *stack = malloc(sizeof(Stack));

	stack->head = NULL;

	return stack;
}

void stack_destroy(Stack *stack)
{
	Node *temp;

	while (!stack_is_empty(stack)) {
		temp = stack->head->next;
		free(stack->head);
		stack->head = temp;
	}

	free(stack);
}

bool stack_is_empty(Stack *stack)
{
	return stack->head == NULL;
}

Stack *stack_push(Stack *stack, int value)
{
	Node *head = malloc(sizeof(Node));

	head->value = value;
	head->next = stack->head;
	stack->head = head;

	return stack;
}

int stack_pop(Stack *stack)
{
	if (stack_is_empty(stack)) return 0;

	int value = stack->head->value;
	Node *temp = stack->head;
	stack->head = temp->next;
	free(temp);

	return value;
}
