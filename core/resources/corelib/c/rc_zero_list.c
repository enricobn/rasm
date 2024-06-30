#include "rc_zero_list.h"
#include "rasm.h"
#include <stdio.h>
#include <stdlib.h>

struct RCZeroList *zero_list;

void remove_from_zero_list(struct RCZeroList *actual) {
  // printf("remove_from_zero_list\n");
  //  print_one_list(actual);

  struct RCZeroList *prev = actual->prev;
  struct RCZeroList *next = actual->next;
  if (prev != NULL) {
    prev->next = next;
  } else {
    zero_list = next;
  }
  if (next != NULL) {
    next->prev = prev;
  }
  // print_one_list(actual);
  free(actual);
}

void push_zero(struct RasmPointer_ *pointer) {
  // printf("push_zero\n");
  struct RCZeroList *new_zero = malloc(sizeof(struct RCZeroList));
  new_zero->pointer = pointer;
  new_zero->next = zero_list;
  new_zero->prev = NULL;
  pointer->zero = new_zero;
  if (zero_list != NULL) {
    zero_list->prev = new_zero;
  }
  zero_list = new_zero;
}

void free_zero() {
  struct RCZeroList *current = zero_list;
  size_t count = 0;
  while (current != NULL) {
    struct RasmPointer_ *pointer = current->pointer;
    struct RCZeroList *next = current->next;
    free(pointer->address);
    free(pointer);
    free(current);
    current = next;
    count++;
  }

  if (count > 1000) {
    printf("count zero %zu\n", count);
  }

  zero_list = NULL;
}