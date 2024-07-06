#include "fs_allocator.h"
#include <stdlib.h>

struct fs_allocator *fs_allocator_new(size_t size, size_t count) {
  struct fs_allocator *result = malloc(sizeof(struct fs_allocator));
  result->size = size;
  result->count = count;
  result->mem = malloc(count * size);
  result->free = malloc(count * sizeof(void **));

  unsigned long mem = (unsigned long)result->mem;
  void **free = result->free;

  for (int i = 0; i < count; i++) {
    *free = (void *)mem;
    free++;
    mem += size;
  }
  result->last_free = --free;
  return result;
}

void *fs_alloc(struct fs_allocator *allocator) {
  void *result = *allocator->last_free;
  allocator->last_free--;
  return result;
}

void fs_free(struct fs_allocator *allocator, void *address) {
  ++allocator->last_free;
  *allocator->last_free = address;
}