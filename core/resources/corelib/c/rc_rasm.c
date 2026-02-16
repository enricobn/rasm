#include "rc_rasm.h"
#include "events.h"
#include "fs_allocator.h"
#include "rasm.h"
#include "rc_zero_list.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Fixed size allocator for struct RasmPointers
 */
struct fs_allocator *fs_allocator;
/**
 * Fixed allocator for struct Enum
 */
struct fs_allocator *fs_allocator_enum;
static size_t enum_size;
static void *enum_max_mem;

/**
 * Allocates memory of size 'size' on the heap and returns a RasmPointer_
 * containing a pointer to the allocated memory. If 'size' is equal to the size
 * of an enum, it allocates memory on the fs_allocator_enum, otherwise it
 * allocates on the standard heap. The returned RasmPointer_ has count set to 0
 * and zero set to NULL.
 */
struct RasmPointer_ *rasmMalloc(size_t size) {

  free_zero();

  void *address;

  if (size == enum_size) {
    address = fs_alloc(fs_allocator_enum);
  } else {
#ifdef __RASM_MEMORY_DEBUG__
    register_event(EVENT_ALLOC);
#endif
    address = malloc(size);
  }

  struct RasmPointer_ *result = fs_alloc(fs_allocator);
  result->address = address;
  result->count = 0;
  result->zero = NULL;

  return result;
}

/**
 * Frees a RasmPointer_ previously allocated by rasmMalloc.
 * If the pointer to the allocated memory is within the range of the
 * fs_allocator_enum, it frees the memory on the fs_allocator_enum, otherwise it
 * frees the memory on the standard heap. Additionally, it frees the
 * RasmPointer_ itself on the fs_allocator
 */
void rasmFree(struct RasmPointer_ *pointer) {
  if (pointer->address >= fs_allocator_enum->mem &&
      pointer->address < enum_max_mem) {
    fs_free(fs_allocator_enum, pointer->address);
  } else {
#ifdef __RASM_MEMORY_DEBUG__
    register_event(EVENT_FREE);
#endif
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

void freeRasmReferences() {
  destroy_zero_list();
  fs_allocator_destroy(fs_allocator_enum);
  fs_allocator_destroy(fs_allocator);
}

struct RasmPointer_ *addStaticStringToHeap(const char *s) {
  struct RasmPointer_ *result = rasmMalloc(strlen(s) + 1);
  strcpy((char *)result->address, s);
  return result;
}

struct Void_ *deref(struct RasmPointer_ *address) {
#ifdef __RASM_MEMORY_DEBUG__
  register_event_no_details(EVENT_DEREF);
#endif

  if (--address->count == 0) {
    push_zero(address);
  }
  return NULL;
}

struct Void_ *addRef(struct RasmPointer_ *address) {
#ifdef __RASM_MEMORY_DEBUG__
  register_event_no_details(EVENT_ADDREF);
#endif

  if (++address->count == 1) {
    if (address->zero != NULL) {
      remove_from_zero_list(address->zero);
      address->zero = NULL;
    }
  }
  return NULL;
}