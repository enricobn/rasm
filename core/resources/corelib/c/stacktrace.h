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

void init_stacktraces(void *reference_address);
void close_stacktraces();
struct stacktrace *stack_trace(int max_frames);
unsigned long *stack_trace_references(int max_frames);
struct stacktrace *stack_trace_from_references(unsigned long *references);
struct line_info *line_info_from_reference(unsigned long reference);
void free_stacktrace(struct stacktrace **trace_ref);
void print_stack_trace(int max_frames);