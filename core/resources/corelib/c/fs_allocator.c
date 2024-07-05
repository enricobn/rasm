#include "fs_allocator.h"
#include <stdio.h>
#include <stdlib.h>

struct fs_allocator *fs_allocator_new(size_t size, size_t count) {
  struct fs_allocator *result = malloc(sizeof(struct fs_allocator));
  result->size = size;
  result->count = count;
  result->mem = calloc(count, size);
  result->pointers = malloc(count * sizeof(struct fs_pointer));
  result->next_free = result->pointers;

  struct fs_pointer *pointer = result->pointers;
  unsigned long mem = (unsigned long)result->mem;

  for (int i = 0; i < count; i++) {
    pointer->allocated = 0;
    pointer->address = (void *)mem;
    pointer++;
    mem += size;
  }
  return result;
}

void *fs_alloc(struct fs_allocator *allocator) {
  struct fs_pointer *result = allocator->next_free;
  if (result != NULL) {
    allocator->next_free = NULL;
  } else {
    result = allocator->pointers;
    while (result->allocated) {
      // printf("loop\n");
      result++;
    }
  }
  result->allocated = 1;
  return result->address;
}

void fs_free(struct fs_allocator *allocator, void *address) {
  unsigned long slot =
      ((unsigned long)address - (unsigned long)allocator->mem) /
      allocator->size;

  // printf("slot %lu", slot);

  struct fs_pointer *result = allocator->pointers + slot;
  result->allocated = 0;
  allocator->next_free = result;
}