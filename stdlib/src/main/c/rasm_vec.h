#include "rc_rasm.h"
#include <stdlib.h>

#ifndef _RASM_VEC_H
#define _RASM_VEC_H 1

struct Vector {
  size_t length;
  struct RasmPointer_ *values;
};

#endif