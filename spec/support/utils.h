#ifndef UTILS_H
#define UTILS_H

extern int test_status;
void array_sequential(int *array, size_t length);
void array_randomize(int *array, size_t length);
void array_delete(int *array, size_t length, int value);
void test_int(int a, int b, char *message);
void test_bool(bool a, bool b, char *message);

#endif
