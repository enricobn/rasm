#include <stdlib.h>

struct Vector {
  size_t length;
  struct RasmPointer_ *values;
};