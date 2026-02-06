#include "rc_zero_list.h"
#include "events.h"
#include "fs_allocator.h"
#include "rasm.h"
#include "rc_rasm.h"
#include <stdio.h>
#include <stdlib.h>

struct RCZeroList *zero_list = NULL;
struct fs_allocator *fs_zero_list_allocator;

void init_zero_list() {
  fs_zero_list_allocator = fs_allocator_new(sizeof(struct RCZeroList), 1000000);
}

void remove_from_zero_list(struct RCZeroList *actual) {
#ifdef __RASM_MEMORY_DEBUG__
  register_event(EVENT_REMOVE_FROM_ZERO);
#endif

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
  fs_free(fs_zero_list_allocator, actual);
}

void push_zero(struct RasmPointer_ *pointer) {
#ifdef __RASM_MEMORY_DEBUG__
  register_event(EVENT_PUSH_ZERO);
#endif

  struct RCZeroList *new_zero = fs_alloc(fs_zero_list_allocator);
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

  while (current != NULL) {
    struct RasmPointer_ *pointer = current->pointer;
    struct RCZeroList *next = current->next;
    rasmFree(pointer);
    fs_free(fs_zero_list_allocator, current);
    current = next;
  }

  zero_list = NULL;
}