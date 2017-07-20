#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "support/utils.h"
#include "binary-tree/binary_tree.h"

static const size_t size = 50;

static size_t compare_ii;
static int *compare_values;

static void compare_helper(int value)
{
	test_int(compare_values[compare_ii++], value, "Value matches compare value");
}

static void is_empty_test()
{
	BinaryTree *tree = binary_tree_create();

	test_bool(binary_tree_is_empty(tree), true, "Tree is empty");
	binary_tree_insert(tree, 10);
	test_bool(binary_tree_is_empty(tree), false, "Tree is not empty");

	binary_tree_destroy(tree);
}

static void exists_test()
{
	BinaryTree *tree = binary_tree_create();

	int insert[size];
	size_t ii;

	array_randomize(insert, size);

	for (ii = 0; ii < size; ii++) binary_tree_insert(tree, insert[ii]);

	test_bool(binary_tree_exists(tree, size / 2), true, "The value exists in the tree");
	test_bool(binary_tree_exists(tree, size + 1), false, "The value does not exist in the tree");
	test_bool(binary_tree_exists(tree, -1), false, "The value does not exist in the tree");

	binary_tree_destroy(tree);
}

static void insert_test()
{
	BinaryTree *tree = binary_tree_create();

	test_bool(binary_tree_exists(tree, 20), false, "The tree is empty");
	binary_tree_insert(tree, 10);
	test_bool(binary_tree_exists(tree, 20), false, "The value does not exist in the tree");
	binary_tree_insert(tree, 20);
	test_bool(binary_tree_exists(tree, 20), true, "The value exists in the tree");

	binary_tree_destroy(tree);
}

static void in_order_test()
{
	BinaryTree *tree = binary_tree_create();

	int insert[size], compare[size];
	size_t ii;

	array_randomize(insert, size);
	array_sequential(compare, size);
	compare_values = compare;

	for (ii = 0; ii < size; ii++) binary_tree_insert(tree, insert[ii]);

	compare_ii = 0;
	binary_tree_in_order(tree, &compare_helper);
	test_int(compare_ii, size, "it traverses the entire tree");

	compare_ii = 0;
	binary_tree_in_order(tree, &compare_helper);
	test_int(compare_ii, size, "it traverses the entire tree");

	binary_tree_destroy(tree);
}

static void delete_test()
{
	BinaryTree *tree = binary_tree_create();

	int insert[size], delete[size], compare[size];
	size_t ii;

	array_randomize(insert, size);
	array_randomize(delete, size);
	array_sequential(compare, size);
	compare_values = compare;

	for (ii = 0; ii < size; ii++) binary_tree_insert(tree, insert[ii]);

	compare_ii = 0;
	binary_tree_in_order(tree, &compare_helper);
	test_int(compare_ii, size, "it traverses the entire tree");

	for (ii = 0; ii < size; ii++) {
		test_bool(binary_tree_exists(tree, delete[ii]), true, "Exists before delete");
		binary_tree_delete(tree, delete[ii]);
		test_bool(binary_tree_exists(tree, delete[ii]), false, "Ooes not exists after delete");

		array_delete(compare, size, delete[ii]);

		compare_ii = 0;
		binary_tree_in_order(tree, &compare_helper);
		test_int(compare_ii, size - ii - 1, "it traverses the entire tree");
	}

	binary_tree_destroy(tree);
}

int main()
{
	is_empty_test();
	exists_test();
	insert_test();
	in_order_test();
	delete_test();

	printf("\n");
	return test_status;
}
