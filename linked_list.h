#include <stdbool.h>
#include <stdio.h> /* load i/o routines */
#include <string.h>
#include <stdlib.h>
#include <ctype.h> /* load character test routines */
#include <stdio.h>
#include <stdio.h>
#include "code_generation.h"

int loop_count = 0;
int if_count = 0;

typedef struct idnode{
	char name[20];
	int curr_instruction;
	int jump_instruction;
	int exit;
	struct operand variable;
	struct idnode* next;
} idnode;
struct idnode* alist;
struct idnode* loop[999];
struct idnode* if_stack[999];
struct idnode* patch_list;
struct idnode* raise_list;
struct idnode jump_table_addr;


void print_list(idnode * head) {
    idnode * current = head;

    while (current != NULL) {
        printf("%d ", current->curr_instruction);
        current = current->next;
    }
}

int remove_by_index(idnode ** head, int n) {
    int i = 0;
    int retval = -1;
    idnode * current = *head;
    idnode * temp_node = NULL;

    if (n == 0) {
        return pop_list_item(head);
    }

    for (i = 0; i < n-1; i++) {
        if (current->next == NULL) {
            return -1;
        }
        current = current->next;
    }

    temp_node = current->next;
    current->next = temp_node->next;
    free(temp_node);

    return retval;

}

int pop_list_item(idnode ** head) {
    int retval = -1;
    idnode * next_node = NULL;

    if (*head == NULL) {
        return -1;
    }

    next_node = (*head)->next;
    free(*head);
    *head = next_node;

    return retval;
}



