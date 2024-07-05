#include "rasm.h"
#include "fs_allocator.h"
#include "rc_zero_list.h"
#include <stdlib.h>

size_t allocationCount = 0;
struct fs_allocator *fs_allocator;

struct RasmPointer_ *rasmMalloc(size_t size) {

  free_zero();

  if (++allocationCount > 100) {
    // wof_gc(wof_allocator);
    allocationCount = 0;
  }

  void *address = malloc(size);

  // struct RasmPointer_ *result = malloc(sizeof(struct RasmPointer_));
  struct RasmPointer_ *result = fs_alloc(fs_allocator);
  result->address = address;
  result->count = 0;
  result->zero = NULL;

  return result;
}

void rasmFree(struct RasmPointer_ *pointer) {
  free(pointer->address);
  fs_free(fs_allocator, pointer);
  // free(pointer);
}

void initRasmReferences() {
  fs_allocator = fs_allocator_new(sizeof(struct RasmPointer_), 1000000);
}