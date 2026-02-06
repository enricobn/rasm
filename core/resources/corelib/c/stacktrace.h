#pragma once

struct line_info {
  char *filename;
  char *function;
  unsigned int line;
};

struct stacktrace {
  struct line_info **infos;
  unsigned int count;
};

void init_stacktraces();
void close_stacktraces();
struct stacktrace *stack_trace(int max_frames);
void free_stacktrace(struct stacktrace **trace_ref);
void print_stack_trace(int max_frames);