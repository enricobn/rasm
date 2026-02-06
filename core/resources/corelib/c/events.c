#include "events.h"
#include "stacktrace.h"
#include <stdio.h>
#include <string.h>

#define EVENTS_COUNT 1000000

long addref_count[EVENTS_COUNT];
long deref_count[EVENTS_COUNT];
long free_count[EVENTS_COUNT];
long alloc_count[EVENTS_COUNT];
long push_zero_count[EVENTS_COUNT];
long remove_from_zero_count[EVENTS_COUNT];

int endsWith(const char *str1, const char *str2) {
  size_t len1 = strlen(str1);
  size_t len2 = strlen(str2);

  // Check if str1 is long enough to contain str2
  if (len1 < len2) {
    return 1;
  }

  // Compare the last len2 characters of str1 with str2
  return strncmp(str1 + len1 - len2, str2, len2) == 0;
}

void register_event(enum Event event) {
  long *events = NULL;
  switch (event) {
  case EVENT_ADDREF:
    events = addref_count;
    break;
  case EVENT_DEREF:
    events = deref_count;
    break;
  case EVENT_FREE:
    events = free_count;
    break;
  case EVENT_ALLOC:
    events = alloc_count;
    break;
  case EVENT_PUSH_ZERO:
    events = push_zero_count;
    break;
  case EVENT_REMOVE_FROM_ZERO:
    events = remove_from_zero_count;
    break;
  }

  struct stacktrace *trace = stack_trace(10);
  for (int i = 0; i < trace->count; i++) {
    struct line_info *info = trace->infos[i];
    if (endsWith(info->filename, "stdlib_examples_perf.c")) {
      events[info->line]++;
    }
  }
}

void print_events(long *events) {

  for (int i = 0; i < EVENTS_COUNT; i++) {
    long count = events[i];
    if (count > 0) {
      printf("line %d = %ld\n", i, count);
    }
  }
}

void print_all_events() {
  printf("addref_count\n");
  print_events(addref_count);
  printf("\n");
  printf("deref_count\n");
  print_events(deref_count);
  printf("\n");
  printf("free_count\n");
  print_events(free_count);
  printf("\n");
  printf("alloc_count\n");
  print_events(alloc_count);
  printf("\n");
  printf("push_zero_count\n");
  print_events(push_zero_count);
  printf("\n");
  printf("remove_from_zero_count\n");
  print_events(remove_from_zero_count);
}