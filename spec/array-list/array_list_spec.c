#include <stdio.h>
#include <stdbool.h>
#include "support/utils.h"
#include "array-list/array_list.h"

static void array_list_is_empty_test()
{
	ArrayList *list = array_list_create();

	test_bool(array_list_is_empty(list), true, "Array list is empty");
	array_list_push(list, 1);
	test_bool(array_list_is_empty(list), false, "Array list is not be empty");

	array_list_destroy(list);
}

static void array_list_length_test()
{
	ArrayList *list = array_list_create();

	test_int(array_list_length(list), 0, "Array list has a length of 0");
	array_list_push(list, 1);
	test_int(array_list_length(list), 1, "Array list has a length of 1");
	array_list_set(list, 205, 8);
	test_int(array_list_length(list), 206, "Array list has a length of 206 (ony more than last element's index)");

	array_list_destroy(list);
}

static void array_list_get_test()
{
	ArrayList *list = array_list_create();

	array_list_push(list, 1);
	test_int(array_list_get(list, 0), 1, "The first element is 1");
	array_list_set(list, 205, 8);
	array_list_push(list, 9);
	test_int(array_list_get(list, 0), 1, "The first is (still) 1");
	test_int(array_list_get(list, 100), 0, "Elements not explicitly set are 0");
	test_int(array_list_get(list, 205), 8, "The 205th element is 8");
	test_int(array_list_get(list, array_list_length(list) - 1), 9, "The last element is 9");

	array_list_destroy(list);
}

static void array_list_set_test()
{
	ArrayList *list = array_list_create();

	array_list_set(list, 205, 8);
	test_int(array_list_get(list, 205), 8, "The 205th element is 8");

	array_list_destroy(list);
}

static void array_list_push_test()
{
	ArrayList *list = array_list_create();

	array_list_push(list, 4);
	test_int(array_list_get(list, 0), 4, "The first element is 4");

	array_list_destroy(list);
}

int main()
{
	array_list_is_empty_test();
	array_list_length_test();
	array_list_get_test();
	array_list_set_test();
	array_list_push_test();

	printf("\n");

	return test_status;
}
