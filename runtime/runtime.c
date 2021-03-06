#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

int debug = 0;

// Garbage collection methods
void initialize(uint64_t rootstack_size, uint64_t heap_size);
void collect(int64_t* rootstack_ptr, uint64_t bytes_requested, int64_t request_no);
void copy (int64_t** rp);

// Print methods
void print_int(int64_t i, short newline);
void print_char(int64_t i, short newline);
void print_bool(int64_t i, short newline);
void print_void(short newline);
void print_array(int64_t* v, int64_t* tag, short newline);
void print_vector(int64_t* v, int64_t* tag, short newline);
void print_function(int64_t* tag, short newline);
void print_plus(int64_t* v, int64_t* tag, short newline);
void print_user(int64_t* v, int64_t* tag, short newline);
void print_any(int64_t val, int64_t* tag, short newline);
void print_any_type (int64_t* tag, short newline);

void show_space (int64_t* start, int64_t* end, char* label);

// Error methods
void array_access_error (int64_t array_length, int64_t index);
void match_error ();

// Compiler determines number associated with type
extern int64_t tint;
extern int64_t tchar;
extern int64_t tbool;
extern int64_t tvoid;
extern int64_t tvector;
extern int64_t tfunc;
extern int64_t tarray;
extern int64_t tplus;
extern int64_t tuser;
extern int64_t tforall;
extern int64_t tfix;
extern int64_t tvar;

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

    *begin = malloc(size * 8);

    if (!*begin) {
        fprintf(stderr, "failed to allocate %lld bytes for %s", size, label);
        exit(1);
    }

    *end = *begin + size;

    memset (*begin, 0, size);

    return;
}

void print_info () {
    printf ("Initialize\n\tFrom Space: %lld - %lld\n\tTo Space: %lld - %lld\n\tRoot Stack: %lld - %lld\n",
        (int64_t) fromspace_begin, (int64_t) fromspace_end, (int64_t)tospace_begin, (int64_t)tospace_end, (int64_t)rootstack_begin, (int64_t)rootstack_end);
    printf ("\tRoot Stack Size: %lld\n\tHeap Size: %lld\n", rootstack_size, heap_size);
    printf ("\tFree Pointer: %lld\n\tRoot Stack Pointer: %lld\n", (int64_t)free_ptr, (int64_t)rootstack_ptr);
    printf ("\tType Int: %lld\n\tType Bool: %lld\n\tType Void: %lld\n\tType Vector: %lld\n",
        (int64_t)&tint, (int64_t)&tbool, (int64_t)&tvoid, (int64_t)&tvector);
}

/*
    Initialize: Allocates space for the from-space, to-space, and root stack.
    @param rs - the size of the root stack
    @param hs - the configured size of the heap
 */
void initialize(uint64_t rs, uint64_t hs) {

    rootstack_size = rs > 0 ? rs : hs / 4;
    heap_size = hs;

    initialize_space("fromspace", &fromspace_begin, &fromspace_end, heap_size);
    initialize_space("tospace", &tospace_begin, &tospace_end, heap_size);
    initialize_space("rootstack", &rootstack_begin, &rootstack_end, rootstack_size);

    free_ptr = fromspace_begin;
    rootstack_ptr = rootstack_begin;

    if (debug) { print_info(); }

    return;
}


// Address is a forwarding ptr if its a number within the to-space
int is_forwarding_ptr (int64_t tag) {
    return ((int64_t*)tag >= tospace_begin && (int64_t*)tag < tospace_end);
}

// Is number within the from-space address range
int is_fromspace_ptr (int64_t ptr) {
    return ( (int64_t*)ptr >= fromspace_begin && (int64_t*)ptr < fromspace_end );
}


void process(int64_t** qp) {

    int64_t* node   = *qp;
    int64_t* tag    = (int64_t*) node[0];      // Datatype of pointer

    if (debug) {
        printf ("Process node @ %lld\n", (int64_t)node);
        printf ("tag: %lld\n", tag[0]);
    }

    int64_t length = 0;
    if (tag[0] == tvector) {
        length = tag[1] + 1;    // Length of contents, including the tag
    } else if (tag[0] == tarray) {
        length = node[1] + 2;   // Length of contents, including tag and size
    } else {
        // If its not a pointer or it has already been copied then skip
        if (debug) {
            printf ("is not a pointer\n");
            show_space(fromspace_begin, fromspace_end, "fromspace");
            show_space(tospace_begin, tospace_end, "tospace");
            show_space(queue_head, queue_tail, "queue");
            char c = getchar();
        }
        queue_head += 1;
        return;
    }

    int64_t* q_ptr = queue_head;

    if (debug) { printf ("Length of node: %lld\n", length); }

    // Iterate over pointer: If there is an element in from-space: copy
    for (int i = 0; i < length; ++i) {
        if (is_fromspace_ptr (q_ptr[i])) {
            if (debug) { printf ("%lld is_fromspace ptr\n", q_ptr[i]); }
            copy ((int64_t**)&q_ptr[i]);
        } else {
            if (debug) {
                printf ("q_ptr[%d] is in to-space at %lld\n", i, q_ptr[i]);
            }
        }
    }

    *qp = q_ptr;            // Updates the ptr in the popped tuple so they point to the newly copied tuples

    queue_head += length;   // pop node off queue

    return;
}


/*
 * copy - copies a pointer from the from-space to the to-space.
 */
void copy (int64_t** rp) {

    int64_t* from_ptr = *rp;
    int64_t* tag = (int64_t*) from_ptr[0];      // Datatype of pointer
    if (debug) {
        printf ("Ptr Tag: %lld\n", (int64_t)tag);
    }

    // If it has already been copied, change old pointer to new address
    if (is_forwarding_ptr ((int64_t)tag)) {
        if (debug) { printf ("is forwarding ptr\n"); }
        *rp = (int64_t*)tag;
        return;
    }

    int64_t length = 0;
    if (tag[0] == tvector) {
        length = tag[1] + 1;       // Length of contents, including the tag
    } else if (tag[0] == tarray) {
        length = from_ptr[1] + 2;  // Add one to length to copy over the length element
    } else {
        if (debug) { printf ("is not a pointer\n"); }
        return;
    }
    int64_t* to_ptr = queue_tail;       // Start copying to the to-space

    if (debug) { printf ("Ptr Length: %lld\n", length); }

    for (int i = 0; i < length; ++i) {  // Copy all the contents of the ptr
        if (debug) { printf ("Copy %lld to %lld\n", from_ptr[i], (int64_t)to_ptr[i]); }
        to_ptr[i] = from_ptr[i];
    }

    queue_tail += length;               // Update the position of the queue_tail

    from_ptr[0] = (int64_t) to_ptr;     // Change the old tag to the address of the new ptr (setting up forwarding ptr)

    if (debug) {
        printf ("Changed from_ptr[0] to new location: %lld\n", from_ptr[0]);
    }

    *rp = to_ptr;                       // Change the original ptr location to point the newly copied ptr in the to-space

    return;
}


/*
 * swap_spaces: exchanges the from-space and to-space begin & end ptrs
 */
void swap_spaces () {
    int64_t* tmp_begin = tospace_begin;
    int64_t* tmp_end   = tospace_end;
    tospace_begin      = fromspace_begin;
    tospace_end        = fromspace_end;
    fromspace_begin    = tmp_begin;
    fromspace_end      = tmp_end;
}

void show_space (int64_t* start, int64_t* end, char* label) {
    int64_t* ptr = start;
    printf("\n");
    while (ptr != end) {
        printf("%s: loc(%lld) : val(%lld)\n", label, (int64_t)ptr, (int64_t)*ptr);
        ptr++;
    }
    printf ("\n\n");
}

void show_rootstack (const char* label, int64_t *start_at) {
    int64_t *droot = start_at;
    uint64_t i = 0;
    printf("scanning all roots (%s)\n", label);
    while (droot < rootstack_end) {
        printf("root[%llu] = %lld = %lld\n", i, (int64_t)droot, (int64_t)*droot);
        i++;
        droot++;
    }
    printf("\n");
}


void collect(int64_t* new_rs_ptr, uint64_t bytes_requested, int64_t request_no) {

    if (debug) {
        printf ("Collect\n\tRootstack Ptr: %lld\n\tBytes Requested: %lld\n\tRequest No: %lld\n",
            (int64_t)new_rs_ptr, (int64_t)bytes_requested, (int64_t)request_no);
    }

    rootstack_ptr = new_rs_ptr;
    queue_head = queue_tail = tospace_begin;

    if (debug) {
        print_info();
        show_space(fromspace_begin, fromspace_end, "fromspace");
        show_space(tospace_begin, tospace_end, "tospace");
        show_space(queue_head, queue_tail, "queue");
        show_rootstack("rootstack", new_rs_ptr);

        char c = getchar();
        while (c != 'c') {
            c = getchar();
        }

        printf ("\nCopying from root-stack to to-space\n\n");
    }

    // Copy all tuples immediately reachable from root set into to-space
    // to form initial queue
    // TODO: <= ?
    for (int64_t* rp = rootstack_ptr; rp < rootstack_end; rp++) {
        if (is_fromspace_ptr (*rp)) {
            if (debug) { printf ("Copy: %lld\n", (int64_t)rp); }
            copy((int64_t**)rp);
        }
    }

    if (debug) {
        show_space(fromspace_begin, fromspace_end, "fromspace");
        show_space(tospace_begin, tospace_end, "tospace");
        show_space(queue_head, queue_tail, "queue");
        show_rootstack("rootstack", new_rs_ptr);

        char c = 'd';
        while (c != 'c') {
            c = getchar();
        }

        printf ("Processing queue\n\tQ HEAD: %lld\n\tQ TAIL: %lld\n",
            (int64_t)queue_head, (int64_t)queue_tail);
    }
    // Enter loop to process the tuple at front of queue
    while (queue_head != queue_tail) {
        process(&queue_head);
    }

    if (debug) {
        show_space(fromspace_begin, fromspace_end, "fromspace");
        show_space(tospace_begin, tospace_end, "tospace");
        show_space(queue_head, queue_tail, "queue");
        show_rootstack("rootstack", new_rs_ptr);

        char c = 'd';
        while (c != 'c') {
            c = getchar();
        }

        printf ("Updating free ptr and swapping spaces\n");
    }
    // Set up free_pointer for future allocates
    free_ptr = queue_tail;

    // Swap to and from space
    swap_spaces ();

    // Clear to-space memory
    memset (tospace_begin, 0, heap_size);

    if (debug) {
        printf("Collect done\n");
        show_space(fromspace_begin, fromspace_end, "fromspace");
        show_space(tospace_begin, tospace_end, "tospace");
        show_space(queue_head, queue_tail, "queue");
        show_rootstack("rootstack", new_rs_ptr);
        printf ("Fromspace End: %lld\n", (int64_t)fromspace_end);
        printf ("Free Ptr: %lld\n", (int64_t)free_ptr);
        printf ("Space left: %lld\n", (int64_t)fromspace_end - (int64_t)free_ptr);
        printf ("Bytes requested: %lld\n", bytes_requested);
    }

    if ((int64_t)fromspace_end - (int64_t)free_ptr < bytes_requested) {
        printf ("Call %lld: Cannot allocate request for %lld bytes\n", request_no, bytes_requested);
        exit (1);
    }

    return;
}


void print_int(int64_t i, short newline) {
    printf("%lld%s", i, newline ? "\n" : "");
}

void print_char(int64_t i, short newline) {
    printf("%c%s", (char)i, newline ? "\n" : "");
}


void print_bool(int64_t i, short newline) {
    printf("#%c%s", i ? 't' : 'f', newline ? "\n" : "");
}


void print_void(short newline) {
    printf("%s", newline ? "\n" : "");
}

void print_array(int64_t* v, int64_t* tag, short newline) {
    int64_t len     = v[1];
    int64_t* type   = (int64_t*) tag[2];

    printf("#[");
    for (uint64_t i = 0; i < len; i++) {
        print_any(v[2 + i], (int64_t*) tag[1], 0);
        if (i + 1 < len) {
            printf (", ");
        }
    }
    printf("]%s", newline ? "\n" : "");
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

void print_function(int64_t* tag, short newline) {
    int64_t  arg_count  = tag[1];
    int64_t* ret        = (int64_t*) tag[arg_count + 2];

    printf("(");
    for (uint64_t i = 0; i < arg_count; i++) {
        print_any_type((int64_t*) tag[2 + i], 0);
        printf (" -> ");
    }
    print_any_type (ret, 0);
    printf(")%s", newline ? "\n" : "");
}

void print_plus(int64_t* v, int64_t* tag, short newline) {
    if (v[1] == 0) {
        print_any (v[2], (int64_t*) tag[2], 0);
    } else {
        print_any (v[3], (int64_t*) tag[3], 0);
    }
}

void print_forall(int64_t* v, int64_t* tag, short newline) {
    print_any(*v, (int64_t*) tag[3], newline);
}

void print_fix(int64_t* v, int64_t* tag, short newline) {
    print_any(*v, (int64_t*) tag[2], newline);
}

void print_user(int64_t* v, int64_t* tag, short newline) {
    char* id = (char*) tag[1];
    printf("(%s ", id);
    print_any((int64_t)v, (int64_t*)tag[2], 0);
    printf(")%s", newline ? "\n" : "");
}


void print_type_int (short newline) {
    printf("Int%s", newline ? "\n" : "");
}

void print_type_char (short newline) {
    printf("Char%s", newline ? "\n" : "");
}


void print_type_bool (short newline) {
    printf("Bool%s", newline ? "\n" : "");
}


void print_type_void (short newline) {
    printf("Void%s", newline ? "\n" : "");
}


void print_type_vector (int64_t* tag, short newline) {
    printf("(");
    for (uint64_t i = 0; i < tag[1]; i++) {
        print_any_type((int64_t*) tag[2 + i], 0);
        if (i + 1 < tag[1]) {
            printf (" * ");
        }
    }
    printf(")%s", newline ? "\n" : "");
}

void print_type_array (int64_t* tag, short newline) {
    printf("(Array ");
    print_any_type((int64_t*) tag[1], 0);
    printf(")%s", newline ? "\n" : "");
}

void print_type_plus (int64_t* tag, short newline) {
    char* id = (char*) tag[1];
    printf("%s%s", id, newline ? "\n" : "");
}

void print_type_user (int64_t* tag, short newline) {
    char* id = (char*) tag[1];
    printf("%s%s", id, newline ? "\n" : "");
}

void print_type_var (int64_t* tag, short newline) {
    char* id = (char*) tag[1];
    printf("%s%s", id, newline ? "\n" : "");
}

void print_type_forall (int64_t* tag, short newline) {
    char* id = (char*) tag[1];
    printf("(Forall %s ", id);
    print_any_type((int64_t*) tag[3], 0);
    printf(")%s", newline ? "\n" : "");
}

void print_type_fix (int64_t* tag, short newline) {
    char* id = (char*) tag[1];
    printf("(Fix %s ", id);
    print_any_type((int64_t*) tag[1], 0);
    printf(")%s", newline ? "\n" : "");
}

void print_any_type (int64_t* tag, short newline) {
    if (tag[0] == tint) {
        print_type_int (newline);
    } else if (tag[0] == tchar) {
        print_type_char (newline);
    } else if (tag[0] == tbool) {
        print_type_bool (newline);
    } else if (tag[0] == tvoid) {
        print_type_void (newline);
    } else if (tag[0] == tvector) {
        print_type_vector (tag, newline);
    } else if (tag[0] == tfunc) {
        print_function (tag, newline);
    } else if (tag[0] == tarray) {
        print_type_array (tag, newline);
    } else if (tag[0] == tplus) {
        print_type_plus (tag, newline);
    } else if (tag[0] == tuser) {
        print_type_user (tag, newline);
    } else if (tag[0] == tforall) {
        print_type_forall (tag, newline);
    } else if (tag[0] == tfix) {
        print_type_fix (tag, newline);
    } else if (tag[0] == tvar) {
        print_type_var (tag, newline);
    } else {
        fprintf(stderr, "Error: print_any_type() - Unknown type in tag[0]: %lld\n", tag[0]);
    }
}


void print_any(int64_t val, int64_t* tag, short newline) {
    if (tag[0] == tint) {
        print_int(val, newline);
    } else if (tag[0] == tchar) {
        print_char(val, newline);
    } else if (tag[0] == tbool) {
        print_bool(val, newline);
    } else if (tag[0] == tvoid) {
        print_void(newline);
    } else if (tag[0] == tvector) {
        print_vector((int64_t*)val, tag, newline);
    } else if (tag[0] == tfunc) {
        print_function (tag, newline);
    } else if (tag[0] == tarray) {
        print_array((int64_t*)val, tag, newline);
    } else if (tag[0] == tplus) {
        print_plus((int64_t*)val, tag, newline);
    } else if (tag[0] == tforall) {
        print_forall((int64_t*)val, tag, newline);
    } else if (tag[0] == tfix) {
        print_fix((int64_t*)val, tag, newline);
    } else if (tag[0] == tuser) {
        print_user((int64_t*)val, tag, newline);
    } else {
        fprintf(stderr, "Error: print_any() - Unknown type in tag[0]: %lld\n", tag[0]);
    }
}


int64_t read_int() {
    int64_t i;
    scanf("%lld", &i);
    return i;
}

void array_access_error (int64_t array_length, int64_t index) {
    fprintf(stderr, "Array Index Error: Array has %lld elements, cannot access index %lld\n", array_length, index);
    abort();
}

void match_error () {
    fprintf(stderr, "Error: Pattern matching failed\n");
    abort();
}
