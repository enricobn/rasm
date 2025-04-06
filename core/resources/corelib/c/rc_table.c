#include "rc_table.h"
#include <stdio.h>
#include <stdlib.h>

static size_t rc_hash(struct RCTable *table, void *pointer);
static struct RCEntry *rc_entry(struct RCTable *table, void *pointer);
static size_t rc_entry_index(struct RCTable *table, void *pointer);
static void push_zero(struct RCTable *table, struct RCEntry *entry);
static struct RCEntry *pop_zero(struct RCTable *table);
static void remove_from_zero_list(struct RCTable *table, struct RCEntry *entry);
static void free_zero(struct RCTable *table);
static void print_one_list(struct RCList *actual);

/*
  Creates a table
*/
struct RCTable *rc_table(size_t buckets_count, size_t bucket_size) {
  struct RCTable *table = malloc(sizeof(struct RCTable));
  table->bucket_size = bucket_size;
  table->buckets_count = buckets_count;
  table->buckets = calloc(bucket_size * buckets_count, sizeof(struct RCEntry));
  table->zero_count = NULL;

  // printf("size %d\n", bucket_size * buckets_count * sizeof(struct RCEntry));

  /*
    for (size_t i = 0; i < buckets_count * bucket_size; i++) {
      table->buckets[i] = NULL;
    }
    */
  return table;
}

/*
  Gets the reference count for the given pointer
*/
int rc_count(struct RCTable *table, void *pointer) {
  struct RCEntry *entry = rc_entry(table, pointer);

  if (entry != NULL) {
    return entry->count;
  }

  return INT_MIN;
}

/*
  Decrements the reference count for the given pointer and returns the new
  count. Returns INT_MIN if the pointer is not found
*/
int rc_dec(struct RCTable *table, void *pointer) {
  struct RCEntry *entry = rc_entry(table, pointer);

  if (entry != NULL) {
    int count = --entry->count;
    if (count == 0) {
      push_zero(table, entry);
    }
    return count;
  }
  return INT_MIN;
}

/*
  Increments the reference count for the given pointer and returns the new
  count. If the pointer is not found in then table then it is added with
  count 1. Returns INT_MIN if the pointer is not found and there's no space to
  add a new pointer in the table.
*/
int rc_inc(struct RCTable *table, void *pointer) {
  struct RCEntry *entry = rc_entry(table, pointer);

  if (entry == NULL) {
    entry = malloc(sizeof(struct RCEntry));
    entry->pointer = pointer;
    entry->count = 1;

    size_t hash = rc_hash(table, pointer);

    size_t i = 0;
    while (i < table->bucket_size) {
      size_t index = hash * table->bucket_size + i;
      struct RCEntry *existing_entry = table->buckets[index];
      if (existing_entry == NULL) {
        entry->index_in_buckets = index;
        entry->zero = NULL;
        table->buckets[index] = entry;
        return 1;
      }
      i++;
    }
    return INT_MIN;
  } else {
    if (entry->count == 0) {
      remove_from_zero_list(table, entry);
    }
    return ++entry->count;
  }
}

/*
  Frees the table, but not the memory of the pointers.
*/
void rc_free(struct RCTable *table) {
  for (size_t i = 0; i < table->buckets_count * table->bucket_size; i++) {
    if (table->buckets[i] != NULL) {
      free(table->buckets[i]);
    }
  }
  free(table->buckets);
  free_zero(table);
  free(table);
}

/*
    Frees the table and the memory of the pointers.
*/

void rc_deep_free(struct RCTable *table) {
  for (size_t i = 0; i < table->buckets_count * table->bucket_size; i++) {
    if (table->buckets[i] != NULL) {
      free(table->buckets[i]->pointer);
      free(table->buckets[i]);
    }
  }
  free(table->buckets);
  free_zero(table);
  free(table);
}

/*
    Removes the pointer from the table
    Returns INT_MIN if the pointer is not in the table, otherwise 0.
*/
int rc_remove(struct RCTable *table, void *pointer) {
  size_t index = rc_entry_index(table, pointer);

  if (index == INT_MIN) {
    return INT_MIN;
  }

  if (table->buckets[index]->count == 0) {
    remove_from_zero_list(table, table->buckets[index]);
  }

  free(table->buckets[index]);
  table->buckets[index] = NULL;
  return 0;
}

/*
  Removes the first address with zero reference count from the table and
  returns it. Returns NULL if there are no addresses with zero reference counts.
*/
void *rc_remove_zero(struct RCTable *table) {
  if (table->zero_count != NULL) {
    struct RCEntry *entry = table->zero_count->entry;
    void *result = entry->pointer;
    remove_from_zero_list(table, entry);
    size_t index_in_buckets = entry->index_in_buckets;
    free(table->buckets[index_in_buckets]);
    table->buckets[index_in_buckets] = NULL;
    // printf("remove_zero %p\n", result);
    return result;
  }
  return NULL;
}

void *rc_print_list(struct RCTable *table) {
  struct RCList *actual = table->zero_count;

  printf("\nTable\n------\n");
  while (actual != NULL) {
    print_one_list(actual);

    actual = actual->next;
  }
  return NULL;
}

static void print_one_list(struct RCList *actual) {
  printf("%p -> entry=%p prev=%p next=%p", actual, actual->entry, actual->prev,
         actual->next);
  if (actual->entry != NULL) {
    printf(" entry->zero=%p\n", actual->entry->zero);
  } else {
    printf("\n");
  }
}

/*
  from
  https://stackoverflow.com/questions/53110781/whats-the-fastest-portable-way-to-hash-pointers-we-know-are-pointer-aligned-to
*/
static size_t rc_hash(struct RCTable *table, void *pointer) {
  unsigned long key = (unsigned long)pointer;
  key ^= (key >> 33);
  key *= 0xff51afd7ed558ccd;
  key ^= (key >> 33);
  key *= 0xc4ceb9fe1a85ec53;
  key ^= (key >> 33);
  return key % table->buckets_count;
}

static struct RCEntry *rc_entry(struct RCTable *table, void *pointer) {
  size_t hash = rc_hash(table, pointer);

  // printf("hash %d\n", hash);

  size_t base_index = hash * table->bucket_size;

  size_t i = 0;
  while (i < table->bucket_size) {
    struct RCEntry *entry = table->buckets[base_index + i];
    // printf("i %d\n", i);
    // printf("entry %p\n", entry);
    if (entry != NULL && entry->pointer == pointer) {
      return entry;
    }
    i++;
  }
  return NULL;
}

static size_t rc_entry_index(struct RCTable *table, void *pointer) {
  size_t hash = rc_hash(table, pointer);

  // printf("hash %d\n", hash);

  size_t i = 0;
  while (i < table->bucket_size) {
    struct RCEntry *entry = table->buckets[hash * table->bucket_size + i];
    // printf("i %d\n", i);
    // printf("entry %p\n", entry);
    if (entry != NULL && entry->pointer == pointer) {
      return hash * table->bucket_size + i;
    }
    i++;
  }
  return INT_MIN;
}

static void push_zero(struct RCTable *table, struct RCEntry *entry) {
  struct RCList *new_zero = malloc(sizeof(struct RCList));
  new_zero->entry = entry;
  new_zero->next = table->zero_count;
  new_zero->prev = NULL;
  entry->zero = new_zero;
  if (table->zero_count != NULL) {
    table->zero_count->prev = new_zero;
  }
  table->zero_count = new_zero;
}

static struct RCEntry *pop_zero(struct RCTable *table) {
  if (table->zero_count == NULL) {
    return NULL;
  }
  struct RCEntry *result = table->zero_count->entry;
  struct RCList *new_zero = table->zero_count->next;
  if (new_zero != NULL) {
    new_zero->prev = NULL;
  }
  free(table->zero_count);
  table->zero_count = new_zero;
  return result;
}

/*
  Searches the pointer in the zero List and removes it.
  TODO return an error value (INT_MIN?) if the pointer is not in the zero
  List.
*/
static void remove_from_zero_list(struct RCTable *table,
                                  struct RCEntry *entry) {
  struct RCList *actual = entry->zero;
  // printf("\nremove_from_zero_list ");
  // print_one_list(actual);

  struct RCList *prev = actual->prev;
  struct RCList *next = actual->next;
  if (prev != NULL) {
    prev->next = next;
  }
  if (next != NULL) {
    next->prev = prev;
  }
  if (prev == NULL) {
    table->zero_count = next;
  }
  entry->zero = NULL;
  // print_one_list(actual);
  free(actual);
}

static void free_zero(struct RCTable *table) {
  while (pop_zero(table) != NULL) {
  }
}