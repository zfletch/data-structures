#ifndef STACK_H
#define STACK_H

typedef struct Stack Stack;
Stack *stack_create(void);
void stack_destroy(Stack *stack);
bool stack_is_empty(Stack *stack);
int stack_pop(Stack *stack);
Stack *stack_push(Stack *stack, int value);

#endif
