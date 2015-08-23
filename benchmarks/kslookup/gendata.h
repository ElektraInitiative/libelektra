#ifndef BENCHMARKS_KSLOOKUP_GENDATA_H
#define BENCHMARKS_KSLOOKUP_GENDATA_H

#define ALPHABET "abcedfghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
#define MIN_DEPTH 3
#define MAX_DEPTH 10
#define MIN_WORDLENGTH 3
#define MAX_WORDLENGTH 15

KeySet * generateKeySet (size_t size);

char getRandomChar (void);
char * generateKeyName (KeySet * checkIfUnique);

#endif
