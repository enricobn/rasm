#include "rasm.h"
#include "fs_allocator.h"
#include "rc_zero_list.h"
#include <stdlib.h>

struct fs_allocator *fs_allocator;
struct fs_allocator *fs_allocator_enum;
static size_t enum_size;
static void *enum_max_mem;

struct RasmPointer_ *rasmMalloc(size_t size) {

  free_zero();

  void *address;

  if (size == enum_size) {
    address = fs_alloc(fs_allocator_enum);
  } else {
    address = malloc(size);
  }

  struct RasmPointer_ *result = fs_alloc(fs_allocator);
  result->address = address;
  result->count = 0;
  result->zero = NULL;

  return result;
}

void rasmFree(struct RasmPointer_ *pointer) {

  if (pointer->address >= fs_allocator_enum->mem &&
      pointer->address < enum_max_mem) {
    fs_free(fs_allocator_enum, pointer->address);
  } else {
    free(pointer->address);
  }
  fs_free(fs_allocator, pointer);
}

void initRasmReferences() {
  init_zero_list();
  fs_allocator = fs_allocator_new(sizeof(struct RasmPointer_), 1000000);
  fs_allocator_enum = fs_allocator_new(sizeof(struct Enum), 1000000);

  enum_size = sizeof(struct Enum);
  enum_max_mem = (void *)((unsigned long)fs_allocator_enum->mem +
                          fs_allocator_enum->count * fs_allocator_enum->size);
}