# DESIGN ##

This document describes the design of the c-api and gives
hints for binding writers. It does not aim to plugin writers
because this detail is hidden from the programmer and is
Elektra specific.

Elektra follows the design principles to make it hard to use
it wrong and fully aims towards an easy to use API for
programmers reading and writing configuration. The data structures
are optimized to get, set and lookup values easily and fast.

The idea is that the KDB API is not only implemented by Elektra.
Elektra provides a full blown architecture to really support
modern Linux Systems, but comes with some overhead. In this
document the KDB API is described. But sometimes there are
hints for elektra specific conventions.


## Data Structures ##

Key, KeySet and KDB datastructures are defined in kdbprivate.h
to remain ABI compatible when something is added to a struct. That means
it is not possible to put one of elektra's datastructures on the stack.
You must use the memory management facilities mentioned in the next chapter.


## Memory Management ##

Elektra manages memory itself. No free must be required, which
was not allocated by the programmer himself. This avoids that
free is forgotten, makes the API more beginner-friendly. In
addition to all that malloc and free must have the same libc
version. malloc in a library linked against another libc, but
freed by the application could lead to hard to find bugs.

Some calls have a opposite call to get the structure freed again:  

    KDB * kdbOpen();
will need the function:  

	int kdbClose(KDB *handle);
to get rid of the resources again. It maybe also shut down
connections, so it really must be called at the end of the
program.

	Key *keyNew(const char *keyName, ...);
	int keyDel(Key *key);

	KeySet *ksNew(int alloc, ...);
	int ksDel(KeySet *ks);

These 2 pairs just malloc what is necessary and free it again.
There are more mallocs then just the KDB, Key and KeySet
structures, but they are invisible and they happen implicit
within any of these 3 classes.

Name, Value, Comment can't be handled as easy, because elektra
does not provide a string library. There are 2 ways to access it,
showed on the Comment example:

	char *keyComment(const Key *key);
just returns the comment and does not allow any change of size of
the comment.

	ssize_t keyGetCommentSize(const Key *key);
to see how long the comment is for above function and how much
buffer to allocate for function below. This value can be directly
passed to malloc.

	ssize_t keyGetComment(const Key *key, char *returnedDesc, size_t maxSize);
will write the comment in a buffer maintained by you which is allocated
with at least the size of the function above.


## Variable Arguments ##

The constructors for Key and KeySet take a variable list of arguments
as alternative to keySet* functions and to ksAppendKey(). With that you
can generate any key or keyset in a single c-statement. This can be
done programmatically by keyGenerate or ksGenerate in libelektratools.

To just get a key, use  

	Key *k = keyNew (0);  
and to just get a keyset, use  

	KeySet *k = ksNew(0, KS_END);

The macros va_start and va_end will not be used then. Otherwise pass
a list like described in the documentation.


## Off-by-one ##

Off-by-one errors (OBOE) are avoided by starting all pointers with 0
as usual in C. The size returned by the *GetSize functions
(keyGetValueSize, keyGetCommentSize and keyGetOwnerSize) is exactly
the size needed to be allocated. So if you add 1 to it, too much is
allocated, but no error will occur.


## Minimal Set ##

The functions listed in kdb.h is a minimal set to fully work with
a key database. They are implemented in src/libelektra in ANSI C.

Functions used by backends are implemented in src/backends/helpers.
They need the POSIX interface and can optionally use iconv to make
utf8 conversations.


## Value, String or Binary ##

Some confusion is about value, string or binary. Value is just a name
which does not specify if it is a string or binary. String is a
char array, with a terminating '\0', but Binary is a void array,
not terminated by '\0'. Only strings may be converted to other charsets.
Use the appendant Get functions, to be not depend on that internal facts.

	const void *keyValue(const Key *key);
does not specify whether it is a binary or string, it will just return
the pointer to it. When Key is a string (check with `keyIsString()`) at
least "" will be returned, see below Return Values for strings.
For binary data a NULL pointer is also possible
to distinguish between no data and '\0'.

	ssize_t keyGetValueSize(const Key *key);
does not specify whether it is a binary or string, it will just return
the size which can be passed to malloc to hold the entire value.

	ssize_t keyGetString(const Key *key, char *returnedString, size_t maxSize);
Get the string into a buffer maintained by you.

	ssize_t keySetString(Key *key, const char *newString);
Set the null terminated string.

	ssize_t keyGetBinary(const Key *key, void *returnedBinary, size_t maxSize);
Get the binary data which might contain '\0'.

	ssize_t keySetBinary(Key *key, const void *newBinary, size_t dataSize);
Set the binary data which might contain '\0'. The length is given
by dataSize.


## Return Value ##

There are many different types of return values. What they have in
common is there error behaviour. Every function must return -1 on
error if it returns Integers (like int, ssize_t). If they return a
pointer, 0 (NULL) will show the error. This can't be used for Integers,
because 0 might be a valid size or is used as "false".

Elektra does not use any float or double, so there is no issue there.

But Elektra uses integers for c-string length, reference counting,
keyset length and internal keyset allocations.

The interface always accept size_t and uses internal size_t
which can hold larger numbers then ssize_t. But to indicate an error it must
be always possible to return -1. This is only possible with
int or ssize_t. These two are used for tests and to return a
size.

So the real size of c-strings and buffers is limited to
SSIZE_MAX which must be checked in every function. When it
exceeds that limit -1 (see above Return Value) or NULL pointer
must be returned.


There are some functions which return an internal string:  

	const char *keyName(const Key *key);  
	const char *keyBaseName(const Key *key);  
	const char *keyOwner(const Key *key);  
	const char *keyComment(const Key *key);  

and in the case that (`keyIsBinary(key)==1`) also:  

	const void *keyValue(const Key *key);  

A Null pointer will lead in all that cases that you get back
a Null pointer.

When there is no string you will get back "", that is a pointer to
'\0'. It marks that there is no string. The function to determine
the size will return 1 in that case. That means that 1 is for empty
strings (Nothing except the NULL terminator).

This is not true for keyValue in the case of binary data, because
'\0' in the first byte is a perfect legal binary data. keyGetValueSize()
may also return 0 for that reason.


## Error Handling ##

Error handling will not be implemented with 0.7.0. So it won't be
possible for an application to determine what exactly went wrong.

Elektra does not set errno. If a function you call sets errno, make sure
to set it back to the old value again.

## Naming ##

All Function Names begin with their class name, e.g. kdb, ks or key.
The words are written together with large letters for separation.
This leads to short names, but might be not as good to read.
*Get* and *Set* are used for getters/and setters, *Is* to ask about
a flag or state, *Needs* to ask about state related to database
and e.g. keyRemove to set a flag or state.
For allocation/deallocation the names are c++ stylish,
like the *New functions *Del.

Macros and Enums are written in capital letters. Options start
with KDB_O, errors KDB_ERR, namespaces KEY_NS and key types with
KEY_TYPE.

The data structures start with a capital letter for every part of
the word:  

	KDB ... Key Data Base Handle  
	KeySet ... Key Set  
	Key ... Key  

keyGetUID() and keyGetGID() have upper case letters because ID is commonly
written in upper case letters.

Only singular is used for all names.


Function Names not belonging to one of the three classes are Elektra specific.
Those have the prefix elektra*. They will always be Elektra specific and
won't be implemented by other KDB implementations.


## const ##

Where possible the functions should use const for parameters. Where Key
or KeySet is not modified, const is used. For return values no const is
used, its more disturbing then have any positive effect. The only
exceptions are:

In special:  

	const char *keyName(const Key *key);  
	const char *keyBaseName(const Key *key);  
	const char *keyComment(const Key *key);  
	const void *keyValue(const Key *key);  
	const char *keyString(const Key *key);  
	const Key  *keyGetMeta(const Key *key, const char* metaName)  

These functions are really thought to get something and not to change anything!
Elektra will lose the knowledge if these keys are synchronized or not. So
they are marked const, and you must not cast that away.
