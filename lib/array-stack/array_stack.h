#ifndef ARRAY_STACK_H
#define ARRAY_STACK_H

typedef struct ArrayStack ArrayStack;
ArrayStack *array_stack_create(size_t size);
void array_stack_destroy(ArrayStack *stack);
bool array_stack_is_empty(ArrayStack *stack);
int array_stack_pop(ArrayStack *stack);
bool array_stack_push(ArrayStack *stack, int value);

#endif
