struct RCZeroList {
  struct RasmPointer_ *pointer;
  struct RCZeroList *next;
  struct RCZeroList *prev;
};

void init_zero_list();
void remove_from_zero_list(struct RCZeroList *actual);
void push_zero(struct RasmPointer_ *pointer);
void free_zero();