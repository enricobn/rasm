#pragma once

#include <string.h>

extern long addref_count[];
extern long deref_count[];
extern long free_count[];
extern long alloc_count[];
extern long push_zero_count[];
extern long remove_from_zero_count[];

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

void print_all_events();