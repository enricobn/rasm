#pragma once

#ifdef __cplusplus
extern "C" {
#endif

// Installs a SIGSEGV/SIGBUS handler on an alternate stack to detect stack overflow
// on Linux. On non-Linux platforms this is a no-op. Must be called early in main().
void rasm_init_stack_overflow_handler(void);

#ifdef __cplusplus
}
#endif
