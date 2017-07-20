#ifndef BINARY_TREE_H
#define BINARY_TREE_H

typedef struct ThreadedBinaryTree ThreadedBinaryTree;
ThreadedBinaryTree *threaded_binary_tree_create(void);
bool threaded_binary_tree_is_empty(ThreadedBinaryTree *tree);
bool threaded_binary_tree_exists(ThreadedBinaryTree *tree, int value);
ThreadedBinaryTree *threaded_binary_tree_insert(ThreadedBinaryTree *tree, int value);
void threaded_binary_tree_in_order(ThreadedBinaryTree *tree, void (*fn)(int));
bool threaded_binary_tree_delete(ThreadedBinaryTree *tree, int value);
void threaded_binary_tree_destroy(ThreadedBinaryTree *tree);

#endif
