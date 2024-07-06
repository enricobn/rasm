#include <stdlib.h>
#include <string.h>

// false = 0
struct fs_pointer {
  void *address;
};

struct fs_allocator {
  size_t size;
  size_t count;
  void *mem;
  struct fs_pointer **last_free;
  struct fs_pointer *pointers;
  struct fs_pointer **free;
};

struct fs_allocator *fs_allocator_new(size_t size, size_t count);
void *fs_alloc(struct fs_allocator *allocator);
void fs_free(struct fs_allocator *allocator, void *address);