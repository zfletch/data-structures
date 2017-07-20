#include <stdio.h>
#include <stdbool.h>
#include "support/utils.h"
#include "stack/stack.h"

static void stack_is_empty_test()
{
	Stack *stack = stack_create();

	test_bool(stack_is_empty(stack), true, "Stack is empty");
	stack_push(stack, 5);
	test_bool(stack_is_empty(stack), false, "Stack is not empty");

	stack_destroy(stack);
}

static void stack_pop_test()
{
	int ii;

	Stack *stack = stack_create();

	for (ii = 0; ii < 100; ii++) stack_push(stack, ii);
	for (ii = 99; ii >= 0; ii--) test_int(stack_pop(stack), ii, "Pops from stack");

	test_int(stack_pop(stack), 0, "Pops from empty stack");

	stack_destroy(stack);
}

static void stack_push_test()
{
	Stack *stack = stack_create();

	stack_push(stack_push(stack, 1), 2);
	test_int(stack_pop(stack), 2, "Pops from stack");
	test_int(stack_pop(stack), 1, "Pops from stack");

	stack_destroy(stack);
}

int main()
{
	stack_is_empty_test();
	stack_pop_test();
	stack_push_test();

	printf("\n");
	return test_status;
}
