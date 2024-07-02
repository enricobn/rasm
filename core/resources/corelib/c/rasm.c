#include "rasm.h"
#include "rc_zero_list.h"
#include <stdlib.h>

// size_t allocationCount = 0;

struct RasmPointer_ *rasmMalloc(size_t size) {

  // if (++allocationCount > 100) {
  free_zero();
  // wof_gc(wof_allocator);
  // allocationCount = 0;
  //}

  void *address = malloc(size);

  struct RasmPointer_ *result = malloc(sizeof(struct RasmPointer_));
  result->address = address;
  result->count = 0;
  result->zero = NULL;

  return result;
}

void rasmFree(struct RasmPointer_ *pointer) {
  free(pointer->address);
  free(pointer);
}

void initRasmReferences() {}