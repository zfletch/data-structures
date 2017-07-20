#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include "binary_tree.h"

typedef struct Node {
	struct Node *left;
	struct Node *right;
	int value;
} Node;

typedef struct BinaryTree {
	Node *root;
} BinaryTree;

BinaryTree *binary_tree_create()
{
	BinaryTree *tree = malloc(sizeof(BinaryTree));

	tree->root = NULL;

	return tree;
}

static void binary_tree_destroy_helper(Node *node)
{
	if (!node) return;

	binary_tree_destroy_helper(node->left);
	binary_tree_destroy_helper(node->right);
	free(node);
}

void binary_tree_destroy(BinaryTree *tree)
{
	binary_tree_destroy_helper(tree->root);

	free(tree);
}

bool binary_tree_is_empty(BinaryTree *tree)
{
	return !tree->root;
}

static bool binary_tree_exists_helper(Node *node, int value)
{
	if (!node) return false;
	if (node->value == value) return true;

	return node->value > value
		? binary_tree_exists_helper(node->left, value)
		: binary_tree_exists_helper(node->right, value);
}

bool binary_tree_exists(BinaryTree *tree, int value)
{
	return binary_tree_exists_helper(tree->root, value);
}

static void binary_tree_insert_helper(Node *root, Node *node)
{
	if (root->value > node->value) {
		if (root->left) {
			binary_tree_insert_helper(root->left, node);
		} else {
			root->left = node;
		}
	} else {
		if (root->right) {
			binary_tree_insert_helper(root->right, node);
		} else {
			root->right = node;
		}
	}
}

static Node *binary_tree_node_create(int value)
{
	Node *node = malloc(sizeof(Node));

	node->value = value;
	node->left = NULL;
	node->right = NULL;

	return node;
}

BinaryTree *binary_tree_insert(BinaryTree *tree, int value)
{
	Node *node = binary_tree_node_create(value);

	if (binary_tree_is_empty(tree)) {
		tree->root = node;
	} else {
		binary_tree_insert_helper(tree->root, node);
	}

	return tree;
}

static void binary_tree_in_order_helper(Node *node, void (*fn)(int))
{
	if (!node) return;

	binary_tree_in_order_helper(node->left, fn);
	(*fn)(node->value);
	binary_tree_in_order_helper(node->right, fn);
}

void binary_tree_in_order(BinaryTree *tree, void (*fn)(int))
{
	binary_tree_in_order_helper(tree->root, fn);
}

static int binary_tree_minimum(Node *node)
{
	return node->left ? binary_tree_minimum(node->left) : node->value;
}

static bool binary_tree_delete_helper(Node *node, Node *parent, int value)
{
	if (!node) return false;

	if (node->value > value) {
		return binary_tree_delete_helper(node->left, node, value);
	} else if (node->value < value) {
		return binary_tree_delete_helper(node->right, node, value);
	}

	if (!node->left) {
		if (parent->left == node) {
			parent->left = node->right;
		} else {
			parent->right = node->right;
		}

		free(node);
	} else if (!node->right) {
		if (parent->left == node) {
			parent->left = node->left;
		} else {
			parent->right = node->left;
		}

		free(node);
	} else {
		node->value = binary_tree_minimum(node->right);
		binary_tree_delete_helper(node->right, node, node->value);
	}

	return true;
}

bool binary_tree_delete(BinaryTree *tree, int value)
{
	if (binary_tree_is_empty(tree)) return false;

	bool deleted;
	Node *root = tree->root;
	Node *fake_root = binary_tree_node_create(0);

	fake_root->left = tree->root;
	deleted = binary_tree_delete_helper(root, fake_root, value);

	tree->root = fake_root->left;
	free(fake_root);

	return deleted;
}
