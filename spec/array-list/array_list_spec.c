#include <stdio.h>
#include <stdbool.h>
#include "array-list/array_list.h"

int status = 0;

void test_int(int a, int b, char *message)
{
	if (a == b) {
		printf(".");
		return;
	}

	printf("\n%s (Expected %d got %d)\n", message, b, a);
	status = 1;
}

void test_bool(bool a, bool b, char *message)
{
	if (a == b) {
		printf(".");
		return;
	}

	printf("\n%s (Expected %d got %d)\n", message, b, a);
	status = 1;
}

int main()
{
	ArrayList *list = array_list_create();

	test_bool(array_list_empty(list), true, "Array list should be empty");
	test_int(array_list_length(list), 0, "Array list should have length of 0");

	array_list_add(list, 1);

	test_bool(array_list_empty(list), false, "Array list should not be empty");
	test_int(array_list_length(list), 1, "Array list should have length of 1");
	test_int(array_list_get(list, 0), 1, "The first element should be 1");

	array_list_set(list, 205, 8);

	test_int(array_list_length(list), 206, "Array list should have length of 206 (ony more than last element's index)");

	array_list_add(list, 9);

	test_int(array_list_get(list, 0), 1, "The first element should be 1");
	test_int(array_list_get(list, 100), 0, "Elements not explicitly set should b 0");
	test_int(array_list_get(list, 205), 8, "The 205th element should be 8");
	test_int(array_list_get(list, array_list_length(list) - 1), 9, "The last element should be 9");

	printf("\n");

	return status;
}
