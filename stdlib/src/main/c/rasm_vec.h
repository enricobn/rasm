#pragma once

#include "rc_rasm.h"
#include <stdlib.h>

struct Vector {
  size_t length;
  struct RasmPointer_ *values;
  size_t total_length;
};