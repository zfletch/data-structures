#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct Node {
	int value;
	struct Node *next;
} Node;


typedef struct List {
	struct Node *head;
} List;

List *list_create()
{
	List *list = malloc(sizeof(List));
	list->head = NULL;

	return list;
}

List *list_insert(List *list, int value)
{
	Node *node = malloc(sizeof(Node));

	node->value = value;
	node->next = list->head;
	list->head = node;

	return list;
}

bool list_exists(List *list, int value) {
	Node *node;

	for (node = list->head; node; node = node->next) {
		if (node->value == value) return true;
	}

	return false;
}

bool list_delete(List *list, int value) {
	Node *node = list->head;

	if (node && node->value == value) {
		Node *next = node->next;
		free(node);
		list->head = next;

		return true;
	}

	for (; node && node->next; node = node->next) {
		if (node->next->value == value) {
			Node *next = node->next;
			node->next = next->next;
			free(next);

			return true;
		}
	}

	return false;
}

/* int main() */
/* { */
/* 	List *list = list_create(); */
/* 	list_insert(list, 1); */
/* 	list_insert(list, 2); */
/* 	list_insert(list, 3); */
/* 	list_insert(list, 4); */
/* 	list_delete(list, 3); */
/*  */
/* 	for (Node *node = list->head; node; node = node->next) { */
/* 		printf("%d\n", node->value); */
/* 	} */
/* } */
