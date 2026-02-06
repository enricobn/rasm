#include "events.h"
#include "rasm.h"
#include "stacktrace.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __RASM_MEMORY_DEBUG__
#define EVENTS_COUNT 1000000
#else
#define EVENTS_COUNT 0
#endif

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
    if (endsWith(info->filename, __RASM_MAIN_OUT_FILE__)) {
      events[info->line]++;
    }
  }
}

struct EventLine {
  size_t line;
  long count;
};

int compare_event_line(const void *a, const void *b) {
  struct EventLine *pa = *(struct EventLine **)a;
  struct EventLine *pb = *(struct EventLine **)b;
  if (pa->count > pb->count)
    return -1;
  else if (pa->count < pb->count)
    return 1;
  return 0;
}

void print_events(FILE *file, long *events) {
  struct EventLine **event_lines =
      malloc(EVENTS_COUNT * sizeof(struct EventLine *));
  size_t event_lines_count = 0;

  for (int i = 0; i < EVENTS_COUNT; i++) {
    if (events[i] > 0) {
      event_lines[event_lines_count] = malloc(sizeof(struct EventLine));
      event_lines[event_lines_count]->line = i;
      event_lines[event_lines_count]->count = events[i];
      event_lines_count++;
    }
  }

  qsort(event_lines, event_lines_count, sizeof(struct EventLine *),
        compare_event_line);

  for (size_t i = 0; i < event_lines_count; i++) {
    fprintf(file, "%s/%s:%zu = %ld\n", __RASM_OUT_FOLDER__,
            __RASM_MAIN_OUT_FILE__, event_lines[i]->line,
            event_lines[i]->count);
  }

  // Cleanup
  for (size_t i = 0; i < event_lines_count; i++) {
    free(event_lines[i]);
  }
  free(event_lines);
}

void print_all_events() {
  char *file_name = malloc(sizeof(char) * 1024);

  snprintf(file_name, 1024, "%s/%s-event.txt", __RASM_OUT_FOLDER__,
           __RASM_MAIN_OUT_FILE__);

  FILE *file = fopen(file_name, "w");
  if (file == NULL) {
    free(file_name);
    printf("Error creating file %s\n", file_name);
    return;
  }

  free(file_name);

  fprintf(file, "addref_count\n");
  print_events(file, addref_count);
  fprintf(file, "\n");
  fprintf(file, "deref_count\n");
  print_events(file, deref_count);
  fprintf(file, "\n");
  fprintf(file, "free_count\n");
  print_events(file, free_count);
  fprintf(file, "\n");
  fprintf(file, "alloc_count\n");
  print_events(file, alloc_count);
  fprintf(file, "\n");
  fprintf(file, "push_zero_count\n");
  print_events(file, push_zero_count);
  fprintf(file, "\n");
  fprintf(file, "remove_from_zero_count\n");
  print_events(file, remove_from_zero_count);

  fclose(file);
}