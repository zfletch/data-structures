#include <stdio.h>
#include <stdbool.h>
#include "utils.h"

int test_status;

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
