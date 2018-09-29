#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

int debug = 0;

// Garbage collection methods
void initialize(uint64_t rootstack_size, uint64_t heap_size);
void collect(int64_t* rootstack_ptr, uint64_t bytes_requested, int64_t request_no);

// Print methods
void print_int(int64_t i, short newline);
void print_bool(int64_t i, short newline);
void print_void(short newline);
void print_vector(int64_t* v, int64_t* tag, short newline);
void print_any(int64_t val, int64_t* tag, short newline);


// Compiler determines number associated with type
extern int64_t tint;
extern int64_t tbool;
extern int64_t tvoid;
extern int64_t tvector;

int64_t* rootstack_ptr      = NULL;
int64_t* rootstack_begin    = NULL;
int64_t* rootstack_end      = NULL;
int64_t* free_ptr           = NULL;
int64_t* fromspace_begin    = NULL;
int64_t* fromspace_end      = NULL;
int64_t* tospace_begin      = NULL;
int64_t* tospace_end        = NULL;
int64_t* queue_head         = NULL;
int64_t* queue_tail         = NULL;
uint64_t rootstack_size     = 0;
uint64_t heap_size          = 0;


/*
    Initialize_Space: Allocates space and stores a pointer to the beginning
        and end of the space. Sets all the contents of the space to 0.
    @param label - name of space (for debugging)
    @param begin - pointer for start of space
    @param end - pointer for end of space
    @param size - the size (in bytes) to allocate for the space
 */
void initialize_space(char* label, int64_t** begin, int64_t** end, uint64_t size) {

    *begin = malloc(size);

    if (!*begin) {
        fprintf(stderr, "failed to allocate %lld bytes for %s", size, label);
        exit(1);
    }

    *end = *begin + size;
    if (debug) {
        printf("Allocated %lld bytes for %s\n", size, label);
    }

    memset (*begin, 0, size);

    return;
}


/*
    Initialize: Allocates space for the from-space, to-space, and root stack.
    @param rs - the size of the root stack
    @param hs - the configured size of the heap
 */
void initialize(uint64_t rs, uint64_t hs) {

    rootstack_size = rs;
    heap_size = hs;

    initialize_space("fromspace", &fromspace_begin, &fromspace_end, heap_size);
    initialize_space("tospace", &tospace_begin, &tospace_end, heap_size);
    initialize_space("rootstack", &rootstack_begin, &rootstack_end, rootstack_size);

    if (debug) {
        printf("initialize\n");
        printf("initialized fromspace size: %lld\n\tfrom %lld to %lld\n", hs, fromspace_begin, fromspace_end);
    }

    free_ptr = fromspace_begin;
    rootstack_ptr = rootstack_end;

    return;
}


void process(int64_t** qp) {
    // If node is not a ptr || has been copied - skip
    // Copy all tuples directly reachable from node over to to-space
        // Place them at the back of the queue
    // Updates the ptr in the popped tuple so they point to the newly copied tuples
    // Pop it off queue
    return;
}


void copy (int64_t** rp) {
    // Copy to to-space
    // Store forwarding ptr
    // Mark old tuple to indicate its been visited
}


void collect(int64_t* new_rs_ptr, uint64_t bytes_requested, int64_t request_no) {

    printf("collect\n");
    queue_head = queue_tail = rootstack_begin;

    // If program wants to allocate an object larger than the heap,
    // increase the heap size to accomodate the request
    while (bytes_requested >= heap_size) {
        heap_size = heap_size * 2;
        initialize_space("fromspace", &fromspace_begin, &fromspace_end, heap_size);
        initialize_space("tospace", &tospace_begin, &tospace_end, heap_size);
    }

    // Copy all tuples immediately reachable from root set into to-space
    // to form initial queue
    for (int64_t* rp = rootstack_ptr; rp != rootstack_end; rp++) {
        copy((int64_t**) rp);
    }

    // Enter loop to process the tuple at front of queue
    while (queue_head != queue_tail) {
        process(&queue_head);
    }

    // Clear to-space memory

    return;
}


void print_int(int64_t i, short newline) {
    printf("%lld%s", i, newline ? "\n" : "");
}


void print_bool(int64_t i, short newline) {
    printf("#%c%s", i ? 't' : 'f', newline ? "\n" : "");
}


void print_void(short newline) {
    printf("%s", newline ? "\n" : "");
}


void print_vector(int64_t* v, int64_t* tag, short newline) {
    printf("(");
    for (uint64_t i = 0; i < tag[1]; i++) {
        print_any(v[1 + i], (int64_t*) tag[2 + i], 0);
        if (i + 1 < tag[1]) {
            printf (", ");
        }
    }
    printf(")%s", newline ? "\n" : "");
}


void print_any(int64_t val, int64_t* tag, short newline) {
    if (tag[0] == tint) {
        print_int(val, newline);
    } else if (tag[0] == tbool) {
        print_bool(val, newline);
    } else if (tag[0] == tvoid) {
        print_void(newline);
    } else if (tag[0] == tvector) {
        print_vector((int64_t*)val, tag, newline);
    } else {
        fprintf(stderr, "Error: print_any() - Unknown type in tag[0]: %lld\n", tag[0]);
    }
}


int64_t read_int() {
    int64_t i;
    scanf("%lld", &i);
    return i;
}
