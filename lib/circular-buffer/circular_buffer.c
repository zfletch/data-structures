#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "circular_buffer.h"

typedef struct CircularBuffer {
	int *start;
	int *end;
	int *read;
	int *write;
} ArrayStack;

CircularBuffer *circular_buffer_create(size_t size)
{
	CircularBuffer *buffer = malloc(sizeof(ArrayStack));

	buffer->start = calloc(size, sizeof(int));
	buffer->end = buffer->start + size;
	buffer->read = buffer->start;
	buffer->write = buffer->start;

	return buffer;
}

void circular_buffer_destroy(CircularBuffer *buffer)
{
	free(buffer->start);
	free(buffer);
}

static int *circular_buffer_inc(CircularBuffer *buffer, int *index)
{
	if (++index >= buffer->end) return buffer->start;

	return index;
}

void circular_buffer_enqueue(CircularBuffer *buffer, int value)
{
	*buffer->write = value;
	buffer->write = circular_buffer_inc(buffer, buffer->write);
}

int circular_buffer_dequeue(CircularBuffer *buffer)
{
	int value = *buffer->read;
	buffer->read = circular_buffer_inc(buffer, buffer->read);

	return value;
}
