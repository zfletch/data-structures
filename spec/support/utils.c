#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include "utils.h"

int test_status;

void array_sequential(int *array, size_t length)
{
	size_t ii;
	for (ii = 0; ii < length; ii++) {
		array[ii] = ii;
	}
}

void array_randomize(int *array, size_t length)
{
	array_sequential(array, length);

	size_t ii, rr;
	int temp;

	for (ii = 0; ii < length; ii++) {
		rr = rand() % length;

		temp = array[rr];
		array[rr] = array[ii];
		array[ii] = temp;
	}
}

void array_delete(int *array, size_t length, int value)
{
	size_t ii;
	bool deleted = false;

	for (ii = 0; ii < length; ii++) {
		if (deleted) array[ii - 1] = array[ii];
		if (array[ii] == value) deleted = true;
	}
}

void test_int(int a, int b, char *message)
{
	if (a == b) {
		printf(".");
		return;
	}

	printf("\n%s (Expected %d got %d)\n", message, b, a);
	test_status = 1;
}

void test_bool(bool a, bool b, char *message)
{
	if (a == b) {
		printf(".");
		return;
	}

	printf("\n%s (Expected %d got %d)\n", message, b, a);
	test_status = 1;
}
