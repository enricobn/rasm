#include <string.h>

#ifndef _RC_RASM_H
#define _RC_RASM_H 1

struct RasmPointer_ {
  void *address;
  int count;
  struct RCZeroList *zero;
};

typedef struct Void_ *(*AddRefDerefFunction_)(struct RasmPointer_ *);

struct Enum {
  struct RasmPointer_ *variant;
  int variant_num;
};

struct Void_ {};

struct RasmPointer_ *rasmMalloc(size_t size);

void rasmFree(struct RasmPointer_ *pointer);

void initRasmReferences();

struct RasmPointer_ *addStaticStringToHeap(char *s);

void deref(struct RasmPointer_ *address);

void addRef(struct RasmPointer_ *address);

#endif