#include <stdio.h>
#include <stdbool.h>
#include "support/utils.h"
#include "stack/stack.h"

void stack_is_empty_test()
{
	Stack *stack = stack_create(10);

	test_bool(stack_is_empty(stack), true, "Stack is empty");
	stack_push(stack, 5);
	test_bool(stack_is_empty(stack), false, "Stack is not empty");

	stack_destroy(stack);
}

void stack_pop_test()
{
	int ii;

	Stack *stack = stack_create(10);

	for (ii = 0; ii < 10; ii++) stack_push(stack, ii);
	for (ii = 9; ii >= 0; ii--) test_int(stack_pop(stack), ii, "Pops from stack");

	test_int(stack_pop(stack), 0, "Pops from empty stack");

	stack_destroy(stack);
}

void stack_push_test()
{
	Stack *stack = stack_create(1);

	test_bool(stack_push(stack, 1), true, "Can push to stack with space");
	test_bool(stack_push(stack, 1), false, "Cannot push to full stack");
	test_bool(stack_is_empty(stack), false, "After pushing stack is not empty");

	stack_destroy(stack);
}

int main()
{
	stack_is_empty_test();
	stack_pop_test();
	stack_push_test();

	return test_status;
}
