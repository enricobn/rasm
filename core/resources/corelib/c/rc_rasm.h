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

struct Lambda_ {
  struct RasmPointer_ *lambda_space;
  void *functionPtr;
  struct Void_ *(*addref_function)(struct RasmPointer_ *);
  struct Void_ *(*deref_function)(struct RasmPointer_ *);
};

void rasmFree(struct RasmPointer_ *pointer);

void initRasmReferences();

struct RasmPointer_ *addStaticStringToHeap(const char *s);

struct Void_ *deref(struct RasmPointer_ *address);

struct Void_ *addRef(struct RasmPointer_ *address);

#endif