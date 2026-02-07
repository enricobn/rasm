/*
needs some libs
sudo apt install libiberty-dev binutils-dev libbfd-dev

first version using perplexity (https://www.perplexity.ai)
*/

#define _GNU_SOURCE // Required for dladdr() on Linux
#include <bfd.h>
#include <dlfcn.h> // Defines Dl_info and dladdr()
#include <execinfo.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stacktrace.h"

#define MAX_STACK_FRAMES 128

struct syms_entry {
  const char *dli_fname;
  bfd *abfd;
  asymbol **syms;
  long symcount;
  asection *text_sec;
  bfd_vma sec_vma;
};

struct syms_entries {
  struct syms_entry **entries;
  unsigned int count;
  void *reference_address;
};

static struct syms_entries *syms_entries = NULL;

void init_stacktraces(void *reference_address) {
  syms_entries = malloc(sizeof(struct syms_entries));
  syms_entries->entries = NULL;
  syms_entries->count = 0;
  syms_entries->reference_address = reference_address;

  bfd_init();
}

void close_stacktraces() {
  for (int i = 0; i < syms_entries->count; i++) {
    struct syms_entry *entry = syms_entries->entries[i];
    bfd_close(entry->abfd);
    entry->abfd = NULL;
    free(entry->syms);
    entry->syms = NULL;
    free(entry);
    entry = NULL;
  }
  free(syms_entries->entries);
  syms_entries->entries = NULL;
  free(syms_entries);
  syms_entries = NULL;
}

void free_line_info(struct line_info **info_ref) {
  struct line_info *info = *info_ref;
  if (info && info->filename) {
    free(info->filename);
    info->filename = NULL;
  }
  if (info && info->function) {
    free(info->function);
    info->function = NULL;
  }
  free(info);
  info = NULL;
  *info_ref = NULL;
}

void free_stacktrace(struct stacktrace **trace_ref) {
  struct stacktrace *trace = *trace_ref;
  for (int i = 0; i < trace->count; i++) {
    free_line_info(&trace->infos[i]);
  }
  free(trace->infos);
  trace->infos = NULL;
  free(trace);
  trace = NULL;
  *trace_ref = NULL;
}

void lookup_line_info(bfd *abfd, asymbol **syms, void *pc,
                      struct line_info *info, asection *text_sec,
                      bfd_vma sec_vma) {
  if (!abfd || !info || !pc || !syms || !text_sec || !sec_vma) {
    info->filename = strdup("invalid_args");
    info->function = strdup("unknown");
    info->line = 0;
    return;
  }

  Dl_info dl_info;
  bfd_vma base_addr = dladdr(pc, &dl_info) && dl_info.dli_fbase
                          ? (uintptr_t)dl_info.dli_fbase
                          : 0;
  bfd_vma pc_offset = (uintptr_t)pc - base_addr - 1;

  bfd_vma section_offset = pc_offset - sec_vma; // *** CORRECT: VMA math ***

  const char *file = NULL;
  const char *func = NULL;
  unsigned int line = 0;

  if (bfd_find_nearest_line(abfd, text_sec, syms, section_offset, &file, &func,
                            &line)) {
    info->filename = strdup(file ? file : "unknown");
    info->function = strdup(func ? func : "unknown");
    info->line = line;
    return;
  }

  info->filename = strdup("lookup_failed");
  info->function = strdup("unknown");
  info->line = 0;
}

struct syms_entry *load_symbols_for_pc(void *pc) {
  Dl_info dl_info;
  if (!dladdr(pc, &dl_info) || !dl_info.dli_fname) {
    printf("dladdr failed for %p\n", pc);
    return NULL;
  }

  bfd *abfd = bfd_openr(dl_info.dli_fname, NULL);
  if (!abfd || !bfd_check_format(abfd, bfd_object)) {
    printf("Failed to open BFD\n");
    return NULL;
  }

  // FIXED: Proper parentheses for bitwise/logical ops
  flagword flags = bfd_get_file_flags(abfd);
  if (!(flags & HAS_SYMS) && !(flags & HAS_LOCALS)) {
    printf("No symbols in binary\n");
    bfd_close(abfd);
    return NULL;
  }

  // Load REGULAR symbols first
  asymbol **syms = NULL;
  long symcount = 0;
  long storage = bfd_get_symtab_upper_bound(abfd);
  if (storage > 0) {
    syms = malloc(storage);
    symcount = bfd_canonicalize_symtab(abfd, syms);
    // printf("Regular symbols: %ld\n", symcount);
  }

  // Fallback: Load DYNAMIC symbols
  if (symcount == 0) {
    free(syms);
    storage = bfd_get_dynamic_symtab_upper_bound(abfd);
    if (storage > 0) {
      syms = malloc(storage);
      symcount = bfd_canonicalize_dynamic_symtab(abfd, syms);
      // printf("Dynamic symbols: %ld\n", symcount);
    }
  }

  struct syms_entry *entry = malloc(sizeof(struct syms_entry));
  entry->dli_fname = dl_info.dli_fname;
  entry->syms = syms;
  entry->symcount = symcount;
  entry->abfd = abfd;
  // should be the old syms_entries->entries be freed?
  syms_entries->entries =
      realloc(syms_entries->entries,
              sizeof(struct syms_entry) * (syms_entries->count + 1));
  syms_entries->entries[syms_entries->count++] = entry;

  asection *text_sec = bfd_get_section_by_name(abfd, ".text");
  if (!text_sec) {
    return NULL;
  }

  bfd_vma sec_vma = bfd_section_vma(text_sec); // Runtime VMA

  entry->text_sec = text_sec;
  entry->sec_vma = sec_vma;

  // printf("Total symbols loaded: %ld\n", symcount);
  return entry;
}

struct syms_entry *find_or_create_sym_entry(void *pc) {
  Dl_info dl_info;
  if (!dladdr(pc, &dl_info) || !dl_info.dli_fname) {
    printf("dladdr failed for %p\n", pc);
    return NULL;
  }

  for (int i = 0; i < syms_entries->count; i++) {
    struct syms_entry *entry = syms_entries->entries[i];
    if (strcmp(entry->dli_fname, dl_info.dli_fname) == 0) {
      return entry;
    }
  }

  return load_symbols_for_pc(pc);
}

struct stacktrace *stack_trace(int max_frames) {
  void *addrs[128];
  int count = backtrace(addrs, max_frames < 128 ? max_frames : 128);
  // char **symbols = backtrace_symbols(addrs, count);

  struct stacktrace *trace = malloc(sizeof(struct stacktrace));
  trace->infos = malloc(sizeof(struct line_info) * count);
  trace->count = 0;
  for (int i = 0; i < count; i++) {
    struct syms_entry *entry = find_or_create_sym_entry(addrs[i]);
    if (entry != NULL) {
      struct line_info *info = malloc(sizeof(struct line_info));
      lookup_line_info(entry->abfd, entry->syms, addrs[i], info,
                       entry->text_sec, entry->sec_vma);
      // ONLY show BFD if it found something useful
      if (strcmp(info->filename, "unknown") != 0 || info->line > 0) {
        trace->infos[trace->count++] = info;
      } else {
        free(info);
      }
    }
  }
  // free(symbols);

  return trace;
}

struct stacktrace *stack_trace_from_references(unsigned long *references) {

  struct stacktrace *trace = malloc(sizeof(struct stacktrace));
  trace->infos = malloc(sizeof(struct line_info) * 128);
  trace->count = 0;

  unsigned long *actual = references;

  while (*actual != 0) {
    struct line_info *info = line_info_from_reference(*actual);
    if (info != NULL) {
      trace->infos[trace->count++] = info;
    }
    actual++;
  }

  return trace;
}

struct line_info *line_info_from_reference(unsigned long reference) {

  void *addr = syms_entries->reference_address + reference;
  struct syms_entry *entry = find_or_create_sym_entry(addr);
  if (entry != NULL) {
    struct line_info *info = malloc(sizeof(struct line_info));
    lookup_line_info(entry->abfd, entry->syms, addr, info, entry->text_sec,
                     entry->sec_vma);
    // ONLY show BFD if it found something useful
    if (strcmp(info->filename, "unknown") != 0 || info->line > 0) {
      return info;
    } else {
      free(info);
      return NULL;
    }
  }
  return NULL;
}

unsigned long *stack_trace_references(int max_frames) {
  int frames = max_frames < MAX_STACK_FRAMES ? max_frames : MAX_STACK_FRAMES;
  // frames + 1 for null terminator
  unsigned long *result = malloc(sizeof(unsigned long) * (frames + 1));
  void **addrs = malloc(sizeof(void *) * frames);
  int count = backtrace(addrs, frames);
  int result_index = 0;
  for (int i = 0; i < count; i++) {
    unsigned long addr = (unsigned long)addrs[i];
    unsigned long reference_address =
        (unsigned long)syms_entries->reference_address;
    if (addr > reference_address) {
      result[result_index++] = addr - reference_address;
    }
  }
  // Add null terminator
  result[result_index] = 0;
  free(addrs);
  return result;
}

void print_stack_trace(int max_frames) {
  printf("Stack trace:\n");
  struct stacktrace *trace = stack_trace(max_frames);

  for (int i = 0; i < trace->count; i++) {
    struct line_info *info = trace->infos[i];
    if (i > 0) {
      printf("%s:%u in %s()\n", info->filename, info->line, info->function);
    }
  }

  free_stacktrace(&trace);
}
