#ifndef STACK_H
#define STACK_H

typedef struct Stack Stack;
Stack *stack_create(size_t size);
void stack_destroy(Stack *stack);
bool stack_is_empty(Stack *stack);
int stack_pop(Stack *stack);
bool stack_push(Stack *stack, int value);

#endif
