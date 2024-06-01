#include <limits.h>
#include <stdlib.h>

struct RCTable {
    size_t bucket_size;
    size_t buckets_count;
    struct RCEntry **buckets;
    struct RCList *zero_count;
};

struct RCEntry {
    void *pointer;
    int count;
};

struct RCList {
    void *pointer;
    struct RCList *next;
};

struct RCTable* rc_table(size_t buckets_count, size_t bucket_size);
int rc_count(struct RCTable *table, void *pointer);
int rc_dec(struct RCTable *table, void *pointer);
int rc_inc(struct RCTable *table, void *pointer);
int rc_remove(struct RCTable *table, void *pointer);
// size_t rc_hash(struct RCTable *table, void *pointer);
// struct RCEntry* rc_entry(struct RCTable *table, void *pointer);
void rc_free(struct RCTable *table);
void rc_deep_free(struct RCTable *table);
// size_t rc_entry_index(struct RCTable *table, void *pointer);
void * rc_remove_zero(struct RCTable *table);