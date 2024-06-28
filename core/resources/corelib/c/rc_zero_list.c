#include "rasm.h"
#include <stdio.h>
#include <stdlib.h>

struct RCZeroList {
  struct RasmPointer_ *pointer;
  struct RCZeroList *next;
  struct RCZeroList *prev;
};

struct RCZeroList *zero_list;

void remove_from_zero_list(struct RCZeroList *actual) {
  // printf("remove_from_zero_list\n");
  //  print_one_list(actual);

  struct RCZeroList *prev = actual->prev;
  struct RCZeroList *next = actual->next;
  if (prev != NULL) {
    prev->next = next;
  }
  if (next != NULL) {
    next->prev = prev;
  }
  if (prev == NULL) {
    zero_list = next;
  }
  // print_one_list(actual);
  free(actual);
}

static struct RasmPointer_ *pop_zero() {
  if (zero_list == NULL) {
    return NULL;
  }
  // printf("pop_zero\n");
  struct RasmPointer_ *result = zero_list->pointer;
  struct RCZeroList *new_zero = zero_list->next;
  if (new_zero != NULL) {
    new_zero->prev = NULL;
  }
  free(zero_list);
  zero_list = new_zero;
  return result;
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
  struct RasmPointer_ *pointer = NULL;
  while ((pointer = pop_zero()) != NULL) {
    // printf("freed %p\n", pointer);
    free(pointer->address);
    free(pointer);
  }
}