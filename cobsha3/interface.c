#include <stdlib.h>
#include <polyglot.h>
#include <libcob.h>

int		SHA3__256 (cob_u8_t *, cob_u8_t *, cob_u8_t *);

void *allocate_character_array(int size) {
    return polyglot_from_i8_array(malloc(size), size);
}

void free_mem(void *p) {
    free(p);
}

void SHA3_256_wrapper(char *input, unsigned long length, char *output) {
    SHA3__256 (input, &length, output);
}

void SHA3_256_wrapper_js_array(char *input, unsigned long length, char *output) {
    SHA3__256 (polyglot_as_i8_array(input), &length, polyglot_as_i8_array(output));
}