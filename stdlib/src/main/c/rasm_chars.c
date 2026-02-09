#include <stdint.h>
#include <string.h>

void uint32_to_utf8_str(uint32_t packed, char *out, size_t out_size) {
  if (out_size < 2) {
    out[0] = '\0';
    return;
  }

  if (packed < 0x80) {
    out[0] = packed;
    out[1] = '\0';
  } else if (packed < 0x800) {
    out[0] = 0xC0 | (packed >> 6);
    out[1] = 0x80 | (packed & 0x3F);
    out[2] = '\0';
  } else if (packed < 0x10000) {
    out[0] = 0xE0 | (packed >> 12);
    out[1] = 0x80 | ((packed >> 6) & 0x3F);
    out[2] = 0x80 | (packed & 0x3F);
    out[3] = '\0';
  } else if (packed <= 0x10FFFF) {
    out[0] = 0xF0 | (packed >> 18);
    out[1] = 0x80 | ((packed >> 12) & 0x3F);
    out[2] = 0x80 | ((packed >> 6) & 0x3F);
    out[3] = 0x80 | (packed & 0x3F);
    out[4] = '\0';
  } else {
    out[0] = '\0'; // Invalid codepoint
  }
}
