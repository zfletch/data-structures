#include <stdio.h>
#include <stdbool.h>
#include "support/utils.h"
#include "array-stack/array_stack.h"

void array_stack_is_empty_test()
{
	ArrayStack *stack = array_stack_create(10);

	test_bool(array_stack_is_empty(stack), true, "ArrayStack is empty");
	array_stack_push(stack, 5);
	test_bool(array_stack_is_empty(stack), false, "ArrayStack is not empty");

	array_stack_destroy(stack);
}

void array_stack_pop_test()
{
	int ii;

	ArrayStack *stack = array_stack_create(10);

	for (ii = 0; ii < 10; ii++) array_stack_push(stack, ii);
	for (ii = 9; ii >= 0; ii--) test_int(array_stack_pop(stack), ii, "Pops from stack");

	test_int(array_stack_pop(stack), 0, "Pops from empty stack");

	array_stack_destroy(stack);
}

void array_stack_push_test()
{
	ArrayStack *stack = array_stack_create(1);

	test_bool(array_stack_push(stack, 1), true, "Can push to stack with space");
	test_bool(array_stack_push(stack, 1), false, "Cannot push to full stack");
	test_bool(array_stack_is_empty(stack), false, "After pushing stack is not empty");

	array_stack_destroy(stack);
}

int main()
{
	array_stack_is_empty_test();
	array_stack_pop_test();
	array_stack_push_test();

	printf("\n");
	return test_status;
}
