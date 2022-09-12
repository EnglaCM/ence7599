#include "hash_table.h"
#include <stdio.h>
#include <stdlib.h>

ioopm_hash_table_t *ioopm_hash_table_create(void) {
    /// Allocate space for a ioopm_hash_table_t = 17 pointers to
    /// entry_t's, which will be set to NULL
    ioopm_hash_table_t *result = calloc(1, sizeof(ioopm_hash_table_t));
    return result;

}

void ioopm_hash_table_destroy(ioopm_hash_table_t *ht) {
    free(ht);
}

