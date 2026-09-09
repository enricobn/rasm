#ifdef __linux__
#define _GNU_SOURCE
#endif
#include "stack_overflow.h"

#ifdef __linux__
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Alternate stack size - SIGSTKSZ is often ~8KB, we allocate larger for safety
// Handler itself must be minimal and async-signal-safe.

static void rasm_stack_overflow_handler(int sig, siginfo_t *si, void *unused) {
    (void)sig;
    (void)si;
    (void)unused;
    const char msg[] = "Fatal error: stack overflow detected\n";
    // write is async-signal-safe, printf is not
    write(STDERR_FILENO, msg, sizeof(msg) - 1);
    _Exit(2);
}

void rasm_init_stack_overflow_handler(void) {
    // Allocate alternate stack for signal handler (normal stack is exhausted)
    stack_t ss;
    ss.ss_sp = malloc(SIGSTKSZ);
    if (ss.ss_sp == NULL) {
        return;
    }
    ss.ss_size = SIGSTKSZ;
    ss.ss_flags = 0;
    if (sigaltstack(&ss, NULL) == -1) {
        free(ss.ss_sp);
        return;
    }

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_sigaction = rasm_stack_overflow_handler;
    sigemptyset(&sa.sa_mask);
    // SA_ONSTACK: run handler on alternate stack
    // SA_SIGINFO: use sa_sigaction instead of sa_handler
    sa.sa_flags = SA_ONSTACK | SA_SIGINFO;

    sigaction(SIGSEGV, &sa, NULL);
    sigaction(SIGBUS, &sa, NULL);
}

#else // non-Linux

void rasm_init_stack_overflow_handler(void) {
    // No-op on non-Linux
}

#endif
