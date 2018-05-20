/* Read a text file with space-separated unsigned byte values and write a binary
 * file with those bytes.
 *
 * Usage:
 *   binarize infile outfile
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/stat.h>

int fail(const char* fmt, ...);

int main(int argc, char** argv)
{
    char* infile;
    char* outfile;
    FILE* input;
    FILE* output;
    char* buf;
    char* p;
    struct stat st;

    argc == 3 || fail("Usage: %s infile outfile\n", argv[0]);
    infile = argv[1];
    outfile = argv[2];
    stat(infile, &st) == 0 || fail("Could not access %s\n", infile);
    (buf = malloc(st.st_size + 1)) != NULL || fail("Out of memory\n");
    (input = fopen(infile, "r")) != NULL || fail("Could not open %s\n", infile);
    fread(buf, 1, st.st_size, input) == st.st_size || fail("Could not read %s\n", infile);
    buf[st.st_size] = 0;
    fclose(input);
    (output = fopen(outfile, "wb")) != NULL || fail("Could not create %s\n", outfile);
    p = buf;
    for(;;) {
        unsigned char c;
        char* endp;
        long x = strtol(p, &endp, 10);
        if (endp == p) {
            !*p || fail("Garbage in the string at position %d: value %d\n", (int)(p - buf), *p);
            break;
        }
        (x >= 0 && x < 256) || fail("Not a byte value at position %d: %ld\n", (int)(p-buf), x);
        c = x;
        fwrite(&c, 1, 1, output) == 1 || fail("Could not write to %s\n", outfile);
        p = endp;
    }
    fclose(output) == 0 || fail("Could not close %s\n", outfile);
    exit(0);
}

int fail(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    exit(1);
    return 0;
}
