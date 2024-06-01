#include "rc_table.h"

// from https://stackoverflow.com/questions/53110781/whats-the-fastest-portable-way-to-hash-pointers-we-know-are-pointer-aligned-to
static size_t rc_hash(struct RCTable *table, void *pointer) {
    unsigned long key = (unsigned long) pointer;
    key ^= (key >> 33);
    key *= 0xff51afd7ed558ccd;
    key ^= (key >> 33);
    key *= 0xc4ceb9fe1a85ec53;
    key ^= (key >> 33);
    return key % table->buckets_count;
}

static struct RCEntry* rc_entry(struct RCTable *table, void *pointer) {
    size_t hash = rc_hash(table, pointer);

    //printf("hash %d\n", hash);

    size_t i = 0;
    while (i < table->bucket_size) {
        struct RCEntry *entry = table->buckets[hash * table->bucket_size + i];
        //printf("i %d\n", i);
        //printf("entry %p\n", entry);
        if (entry != NULL && entry->pointer == pointer) {
            return entry;
        }
        i++;
    }
    return NULL;
}

static size_t rc_entry_index(struct RCTable *table, void *pointer) {
    size_t hash = rc_hash(table, pointer);

    //printf("hash %d\n", hash);

    size_t i = 0;
    while (i < table->bucket_size) {
        struct RCEntry *entry = table->buckets[hash * table->bucket_size + i];
        //printf("i %d\n", i);
        //printf("entry %p\n", entry);
        if (entry != NULL && entry->pointer == pointer) {
            return hash * table->bucket_size + i;
        }
        i++;
    }
    return INT_MIN;
}

static void push_zero(struct RCTable *table, void *pointer) {
    struct RCList *new_zero = malloc(sizeof(struct RCList));
    new_zero->pointer = pointer;
    new_zero->next = table->zero_count;
    table->zero_count = new_zero;
}

static void * pop_zero(struct RCTable *table) {
    if (table->zero_count == NULL) {
        return NULL;
    }
    struct RCList *result = table->zero_count->pointer;
    struct RCList *new_zero = table->zero_count->next;
    free(table->zero_count);
    table->zero_count = new_zero;
    return result;
}

static void remove_zero(struct RCTable *table, void *pointer) {
    struct RCList *actual = table->zero_count;
    struct RCList *prev = NULL;

    while (actual != NULL) {
        if (actual->pointer == pointer) {
            if (prev == NULL) {
                struct RCList *new_zero = table->zero_count->next;
                free(table->zero_count);
                table->zero_count = new_zero;
            } else {
                struct RCList *tmp = actual;
                prev->next = actual->next;
                free(tmp);
            }
            return;
        }
        prev = actual;
        actual = actual->next;
    }
}

static void free_zero(struct RCTable *table) {
    while (pop_zero(table) != NULL) {

    }
}

struct RCTable* rc_table(size_t buckets_count, size_t bucket_size) {
    struct RCTable *table = malloc(sizeof(struct RCTable));
    table->bucket_size = bucket_size;
    table->buckets_count = buckets_count;
    table->buckets = calloc(bucket_size * buckets_count, sizeof(struct RCEntry));
    table->zero_count = NULL;

    // printf("size %d\n", bucket_size * buckets_count * sizeof(struct RCEntry));

    for (size_t i = 0; i < buckets_count * bucket_size; i++) {
        table->buckets[i] = NULL;
    }
    return table;
}

int rc_count(struct RCTable *table, void *pointer) {
    struct RCEntry *entry = rc_entry(table, pointer);

    if (entry != NULL) {
        return entry->count;
    }

    return INT_MIN;
}

int rc_dec(struct RCTable *table, void *pointer) {
    struct RCEntry *entry = rc_entry(table, pointer);

    if (entry != NULL) {
        int count = --entry->count;
        if (count == 0) {
            push_zero(table, pointer);
        }
        return count;
    }
    return INT_MIN;
}

int rc_inc(struct RCTable *table, void *pointer) {
    struct RCEntry *entry = rc_entry(table, pointer);

    if (entry == NULL) {
        entry = malloc(sizeof(struct RCEntry));
        entry->pointer = pointer;
        entry->count = 1;

        size_t hash = rc_hash(table, pointer);

        size_t i = 0;
        while (i < table->bucket_size) {
            struct RCEntry *existing_entry = table->buckets[hash * table->bucket_size + i];
            if (existing_entry == NULL) {
                table->buckets[hash * table->bucket_size + i] = entry;
                return 1;
            }
            i++;
        }
        return INT_MIN;
    } else {
        if (entry->count == 0) {
            remove_zero(table, pointer);
        }
        return ++entry->count;
    }
}

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

int rc_remove(struct RCTable *table, void *pointer) {
    size_t index = rc_entry_index(table, pointer);

    if (index == INT_MIN) {
        return INT_MIN;
    }

    if (table->buckets[index]->count == 0) {
        remove_zero(table, pointer);
    }

    free(table->buckets[index]);
    table->buckets[index] = NULL;
    return 0;
}

void * rc_remove_zero(struct RCTable *table) {
    if (table->zero_count != NULL) {
        void *result = table->zero_count->pointer;
        rc_remove(table, result);
        // printf("remove_zero %p\n", result);
        return result;
    }
    return NULL;
}