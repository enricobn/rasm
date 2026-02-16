enum Event {
  EVENT_ADDREF,           // Defaults to 0
  EVENT_DEREF,            // 1
  EVENT_FREE,             // 2
  EVENT_ALLOC,            // 3
  EVENT_PUSH_ZERO,        // 4
  EVENT_REMOVE_FROM_ZERO, // 5
};

void register_event(enum Event event);
void register_event_no_details(enum Event event);

void print_all_events();