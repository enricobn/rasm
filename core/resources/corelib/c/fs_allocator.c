#include "fs_allocator.h"
#include <stdlib.h>

/**
 * Allocate a new fs_allocator with a given size and count.
 *
 * The allocator is very naive, it does not check if a block has already been
 * freed, those kind of checks, are supposed to be done by the caller.
 *
 * TODO add bounds check and error handling
 *
 * @param size the size of each block in the allocator
 * @param count the number of blocks in the allocator
 * @return a pointer to the newly allocated fs_allocator
 */
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

/**
 * Allocates a block of memory from the given allocator.
 *
 * The memory is not initialized.
 *
 * @param allocator the allocator from which to allocate the memory.
 * @return the address of the allocated memory block.
 */
void *fs_alloc(struct fs_allocator *allocator) {
  void *result = *allocator->last_free;
  allocator->last_free--;
  return result;
}

/**
 * Frees a block of memory previously allocated by fs_alloc.
 *
 * @param allocator the allocator from which to free the memory.
 * @param address the address of the memory to free.
 */
void fs_free(struct fs_allocator *allocator, void *address) {
  ++allocator->last_free;
  *allocator->last_free = address;
}