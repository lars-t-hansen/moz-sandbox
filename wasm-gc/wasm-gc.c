// Non-generational semispace copying collector, fixed heap areas, a single
// global root, a single stack, no pointer tagging, no headerless objects.
//
// How could this be augmented with wasm assists?
//
// Clearly nailing down the object and stack layout would remove a lot of
// abstraction.

#define PAGESIZE 65536
#define FORWARD_MARKER ~0
#define NO_REF ~0

typedef uint32_t u32;
typedef unsigned char u8;

struct stackmap_t {
    u32 length;
    u32 offsets[1];             /* Actually longer */
};

struct refmap_t;
struct refmap_cursor_t;

/* Returns nullptr on failure */
u8* alloc_pages(u32 pages);

struct heap_t {
    /* Allocation pointers */
    u8* alloc;
    u8* limit;

    /* A single root, which is a standard heap object, manipulated by the
       mutator as it needs to. */
    u8* root;

    /* We alloc in newspace, the other is free. */
    u8* newspace;
    u8* newspace_limit;
    u8* oldspace;
    u8* oldspace_limit;
};

void create_heap(heap_t* h, u32 total_memory) {
    u32 semispace_pages = (total_memory / PAGESIZE) / 2;
    semispace_pages > 0 || abort();
    u8* memory = alloc_pages(semispace_pages * 2);
    memory != nullptr || abort();

    u32 semispace_size = semispace_pages * PAGESIZE;
    h->newspace = memory;
    h->newspace_limit = h->newspace + semispace_size;
    h->oldspace = h->newspace_limit;
    h->oldspace_limit = h->oldspace + semispace_size;
    h->alloc = h->newspace;
    h->limit = h->newspace_limit;
}

void forward(heap_t* h, u8** ptrloc);

void collect(heap_t* h, u8* frame) {
    /* Flip */
    u8* tmpspace = h->newspace;
    u8* tmplimit = h->newspace_limit;
    h->newspace = h->oldspace;
    h->newspace_limit = h->oldspace_limit;
    h->oldspace = tmpspace;
    h->oldspace_limit = tmplimit;
    h->alloc = h->newspace;
    h->alloc_limit = h->newspace_limit;

    /* Mutator choice: how to represent and scan non-stack roots */
    forward(h, &h->root);

    /* Forward pointers in the stack.  The stack is hidden from the mutator, so
     * we get to decide how it works, we simply assume that there's some kind of
     * annotation that tells us about where to find pointers.  The initial value
     * of `frame` is the innermost frame.
     *
     * On the other hand, this sort of begs the question - what kind of
     * annotation is sufficient?  At least with a shadow stack we know what's
     * going on.  So this really implements the "stack scanning assist", without
     * the "trace" opcode.
     */
    for (; frame; frame = next_frame(frame)) {
        stackmap_t* stackmap = frame_to_stackmap(frame);
        for (u32 i=0; i < stackmap->length; i++) {
            forward(h, (u8**)(frame + stackmap->offsets[i]));
        }
    }

    /* Forward pointers in newspace. */
    u8* scan = h->current_semispace;
    while (scan < h->alloc) {
        u32 header = *(u32*)scan;
        /* Mutator choice: how to find pointers in the object */
        /* Mutator choice: headerless? */
        /* Mutator choice: BiBoP? */
        refmap_t* refmap = header_to_refmap(header);
        refmap_cursor_t refmap_cursor = init_refmap_scan(refmap);
        for (;;) {
            u32 next_offs = refmap_next(refmap_cursor);
            if (next_offs == NO_REF) {
                break;
            }
            forward(h, (u8**)(scan + next_offs));
        }
        scan += header_to_size(header);
    }
}

void forward(heap_t* h, u8** ptrloc) {
    u8* ptr = *ptrloc;
    if (!ptr || (ptr >= h->newspace && ptr < h->newspace_limit)) {
        return;
    }
    /* Mutator choice: type/size encoding in pointer tags? */
    /* Mutator choice: interior pointers */
    u32 header = *(u32*)ptr;
    if (header == FORWARD_MARKER) {
        *ptrloc = *((u32*)ptr + 1);
        return;
    }
    /* Mutator choice: BiBoP? */
    u32 objsize = header_to_size(header);
    u8* dest = h->alloc;
    h->alloc += objsize;
    memcpy(dest, ptr, objsize);
    *(u32*)ptr = FORWARD_MARKER;
    *((u32*)ptr + 1) = dest;
    *ptrloc = dest;
}


