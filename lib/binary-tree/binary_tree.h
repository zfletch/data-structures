#ifndef BINARY_TREE_H
#define BINARY_TREE_H

typedef struct BinaryTree BinaryTree;
BinaryTree *binary_tree_create(void);
bool binary_tree_is_empty(BinaryTree *tree);
bool binary_tree_exists(BinaryTree *tree, int value);
BinaryTree *binary_tree_insert(BinaryTree *tree, int value);
void binary_tree_in_order(BinaryTree *tree, void (*fn)(int));
bool binary_tree_delete(BinaryTree *tree, int value);
void binary_tree_destroy(BinaryTree *tree);

#endif
