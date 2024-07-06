#include "fs_allocator.h"
#include <stdlib.h>

struct fs_allocator *fs_allocator_new(size_t size, size_t count) {
  struct fs_allocator *result = malloc(sizeof(struct fs_allocator));
  result->size = size;
  result->count = count;
  result->mem = malloc(count * size);
  result->pointers = malloc(count * sizeof(struct fs_pointer));
  result->free = malloc(count * sizeof(struct fs_pointer **));

  struct fs_pointer *pointer = result->pointers;
  unsigned long mem = (unsigned long)result->mem;
  struct fs_pointer **free = result->free;

  for (int i = 0; i < count; i++) {
    pointer->address = (void *)mem;
    *free = pointer;
    free++;
    pointer++;
    mem += size;
  }
  result->last_free = --free;
  return result;
}

void *fs_alloc(struct fs_allocator *allocator) {
  struct fs_pointer *result = *allocator->last_free;
  allocator->last_free--;
  return result->address;
}

void fs_free(struct fs_allocator *allocator, void *address) {
  unsigned long slot =
      ((unsigned long)address - (unsigned long)allocator->mem) /
      allocator->size;

  struct fs_pointer *result = allocator->pointers + slot;
  ++allocator->last_free;
  *allocator->last_free = result;
}