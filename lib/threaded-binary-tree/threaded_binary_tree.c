#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include "threaded_binary_tree.h"

typedef struct Node {
	struct Node *left;
	struct Node *right;
	int value;
} Node;

typedef struct ThreadedBinaryTree {
	Node *root;
} ThreadedBinaryTree;

ThreadedBinaryTree *threaded_binary_tree_create()
{
	ThreadedBinaryTree *tree = malloc(sizeof(ThreadedBinaryTree));

	tree->root = NULL;

	return tree;
}

void threaded_binary_tree_destroy(ThreadedBinaryTree *tree)
{
	Node *root = tree->root;
	Node *previous, *temp;

	while (root) {
		if (root->left) {
			for (previous = root->left;
					previous->right && previous->right != root;
					previous = previous->right);

			if (previous->right == root) {
				for (previous = root->left; previous != root;) {
					temp = previous;
					previous = previous->right;
					free(temp);
				}

				root = root->right;
			} else {
				previous->right = root;
				root = root->left;
			}
		} else {
			previous = root;
			root = root->right;
		}
	}

	root = tree->root;
	while (root) {
		temp = root;
		root = root->right;
		free(temp);
	}

	free(tree);
}

bool threaded_binary_tree_is_empty(ThreadedBinaryTree *tree)
{
	return !tree->root;
}

bool threaded_binary_tree_exists(ThreadedBinaryTree *tree, int value)
{
	if (threaded_binary_tree_is_empty(tree)) return false;

	Node *root = tree->root;

	do {
		if (root->value == value) return true;
	} while (root = root->value > value ? root->left : root->right);

	return false;
}

static void inline threaded_binary_tree_insert_helper(Node *root, Node *node)
{
	while (root) {
		if (root->value > node->value) {
			if (!root->left) {
				root->left = node;
				break;
			}

			root = root->left;
		} else {
			if (!root->right) {
				root->right = node;
				break;
			}

			root = root->right;
		}
	}
}

ThreadedBinaryTree *threaded_binary_tree_insert(ThreadedBinaryTree *tree, int value)
{
	Node *node = malloc(sizeof(Node));

	node->value = value;
	node->left = NULL;
	node->right = NULL;

	if (threaded_binary_tree_is_empty(tree)) {
		tree->root = node;
	} else {
		threaded_binary_tree_insert_helper(tree->root, node);
	}

	return tree;
}

void threaded_binary_tree_in_order(ThreadedBinaryTree *tree, void(*fn)(int))
{
	Node *root = tree->root;
	Node *previous;

	while (root) {
		if (root->left) {
			for (previous = root->left;
					previous->right && previous->right != root;
					previous = previous->right);

			if (previous->right == root) {
				(*fn)(root->value);
				previous->right = NULL;
				root = root->right;
			} else {
				previous->right = root;
				root = root->left;
			}
		} else {
			(*fn)(root->value);
			previous = root;
			root = root->right;
		}
	}
}

static void inline threaded_binary_tree_delete_node_in_place(Node *root)
{
	Node *temp, *min;

	if (!root->right->left) {
		temp = root->right;
		root->value = temp->value;
		root->right = temp->right;

		free(temp);
	} else {
		min = root->right;

		while (min->left) {
			temp = min;
			min = min->left;
		}

		root->value = min->value;
		temp->left = min->right;

		free(min);
	}
}

bool threaded_binary_tree_delete(ThreadedBinaryTree *tree, int value)
{
	if (threaded_binary_tree_is_empty(tree)) return false;

	Node *temp;
	Node *root = tree->root;

	if (root->value == value) {
		if (!root->left) {
			tree->root = root->right;

			free(root);
		} else if (!root->right) {
			tree->root = root->left;

			free(root);
		} else {
			threaded_binary_tree_delete_node_in_place(root);
		}

		return true;
	}

	while (root) {
		if (root->value > value) {
			temp = root;
			root = root->left;
		} else if (root->value < value) {
			temp = root;
			root = root->right;
		} else {
			if (!root->left) {
				if (root == temp->left) {
					temp->left = root->right;
				} else if (root == temp->right) {
					temp->right = root->right;
				}

				free(root);
			} else if (!root->right) {
				if (root == temp->left) {
					temp->left = root->left;
				} else if (root == temp->right) {
					temp->right = root->left;
				}

				free(root);
			} else {
				threaded_binary_tree_delete_node_in_place(root);
			}


			return true;
		}
	}

	return false;
}
