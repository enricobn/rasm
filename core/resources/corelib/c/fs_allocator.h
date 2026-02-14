#pragma once

#include <stdlib.h>
#include <string.h>

/**
 * Fixed size allocator, it is created as a single memory block of size * count
 * bytes.
 *
 * It allocates memory from the first block to the last block.
 *
 * The allocator allocates memory in blocks of a given size and count.
 *
 * @param size the size of each block in the allocator
 * @param count the number of blocks in the allocator
 */
struct fs_allocator {
  size_t size;
  size_t count;
  void *mem;
  void **last_free;
  void **free;
};

struct fs_allocator *fs_allocator_new(size_t size, size_t count);
void fs_allocator_destroy(struct fs_allocator *allocator);
void *fs_alloc(struct fs_allocator *allocator);
void fs_free(struct fs_allocator *allocator, void *address);