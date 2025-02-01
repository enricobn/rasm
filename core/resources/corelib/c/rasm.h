#include <string.h>

struct RasmPointer_ {
  void *address;
  int count;
  struct RCZeroList *zero;
};

struct Enum {
  struct RasmPointer_ *variant;
  int variant_num;
};

struct Void_ {};

struct RasmPointer_ *rasmMalloc(size_t size);

void rasmFree(struct RasmPointer_ *pointer);

void initRasmReferences();