#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H

typedef struct CircularBuffer CircularBuffer;
CircularBuffer *circular_buffer_create(size_t size);
void circular_buffer_destroy(CircularBuffer *stack);
void circular_buffer_enqueue(CircularBuffer *buffer, int value);
int circular_buffer_dequeue(CircularBuffer *buffer);

#endif
