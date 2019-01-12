#include <stdio.h>
#include <stdbool.h>
#include "support/utils.h"
#include "circular-buffer/circular_buffer.h"

void circular_buffer_enqueue_test()
{
	CircularBuffer *buffer = circular_buffer_create(12);

	circular_buffer_enqueue(buffer, 1);
	test_int(circular_buffer_dequeue(buffer), 1, "Enqueue and dequeue value");

	circular_buffer_destroy(buffer);
}

void circular_buffer_wrap_around_test()
{
	int ii;
	CircularBuffer *buffer = circular_buffer_create(12);

	for (ii = 1; ii <= 13; ii++) circular_buffer_enqueue(buffer, ii);

	test_int(circular_buffer_dequeue(buffer), 13, "Enqueue wraps around");
	for (ii = 2; ii <= 12; ii++) {
		test_int(circular_buffer_dequeue(buffer), ii, "Dequeue continues after wrap-around");
	}

	circular_buffer_destroy(buffer);
}

int main()
{
	circular_buffer_enqueue_test();
	circular_buffer_wrap_around_test();

	printf("\n");
	return test_status;
}

