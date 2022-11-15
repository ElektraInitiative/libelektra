# Buffer Struct

## Problem

For this decision a buffer is a fixed-length sequence of arbitrary bytes.
In other words, it is a pointer to data combined with the size of this data.

Many parts of Elektra use such buffers, but there is currently no uniform API for all of these.

## Constraints

## Assumptions

## Considered Alternatives

### Public Buffer struct

Introduce a new struct that is part of the public API and therefore declared in a public header.
The struct should be usable everywhere, so it should be declared by `libelektra-core`.

```c
typedef struct {
    void * data;
    size_t size;
} ElektraBuffer;
```

> **Note**: It is not clear yet, whether `void * data` or `const void * data` is preferrable.

This struct can be embedded into other structs, e.g.:

```c
struct _Key
{
	ElektraBuffer name;
    ElektraBuffer value;

    KeySet * meta;
	keyflag_t flags;
	uint16_t refs;
};
```

The public API for accessing the data could remain unchanged:

```c
const char * keyName (ElektraKey * key) {
    return key->name.data;
}

const size_t keyNameSize (ElektraKey * key) {
    return key->name.size;
}
```

It could also return the buffer in a single function.
Either by value:

```c
const ElektraBuffer keyName (ElektraKey * key) {
    return key->name;
}

void foo(ElektraKey * key) {
    const ElektraBuffer name = keyName (key);
    printf("size %zd, first part: %s", name.size, (const char*)name.data);
}
```

or as a pointer:

```c
const ElektraBuffer * keyName (ElektraKey * key) {
    return &key->name;
}

void foo(ElektraKey * key) {
    const ElektraBuffer * name = keyName (key);
    printf("size %zd, first part: %s", name->size, (const char*)name->data);
}
```

A [short investigation](https://godbolt.org/z/r8q7YfqaW) into the generated assembly code indicates that the pointer version is likely superior.

The buffer struct could also be used as an argument in the public API.
For simple functions that would only take one such `ElektraBuffer` argument there is little benefit.
Especially on the caller side, the code becomes less readable:

```c
void fooNoBuffer(void * data, size_t size);
void fooWithBuffer(ElektraBuffer buffer);
void fooWithBufferPtr(ElektraBuffer * buffer);

void caller(void) {
    size_t size1 = 42;
    void * data1 = malloc(size1);

    fooNoBuffer(data1, size1);
    fooWithBuffer((ElektraBuffer){.data = data1, .size = size1});
    fooWithBufferPtr(&(ElektraBuffer){.data = data1, .size = size1});

    ElektraBuffer buffer; // buffer from somewhere else
    fooNoBuffer(buffer.data, buffer.size);
    fooWithBuffer(buffer);
    fooWithBufferPtr(&buffer);

    ElektraBuffer * bufferPtr; // buffer pointer from somewhere else
    fooNoBuffer(bufferPtr->data, bufferPtr->size);
    fooWithBuffer(*bufferPtr);
    fooWithBufferPtr(bufferPtr);
}
```

However, for functions that need multiple buffer-like inputs the API should be more user-friendly:

```c
Key * keyNew(const void * name, size_t nameSize, const void * value, size_t valueSize);
Key * keyNewWithBuffer(const ElektraBuffer name, const ElektraBuffer value);
Key * keyNewWithBufferPtrs(const ElektraBuffer * name, const ElektraBuffer * value);
```

Furthermore, new APIs could be introduced that operate on such buffer structs.
For example, extensive key name manipulation APIs could be created:

```c
typedef ElektraBuffer ElektraKeyname;

const char * elektraKeynameEscape(const ElektraKeyname * keyname);
int elektraKeynameUnescape(const char * escapedName, ElektraKeyname * unescapedName);

void elektraKeynameAddPart(ElektraKeyname * keyname, const char * part);
void elektraKeynameAddEscaped(ElektraKeyname * keyname, const char * escapedSuffix);

const char * elektraKeynameLastPart(const ElektraKeyname * keyname);

// examples below assume these functions exists
ElektraKeyname * elektraKeyname(ElektraKey * key);
void elektraKeySetName(ElektraKey * key, const void * name, size_t size);

void caller(ElektraKey * key) {
    printf("name: %s\n", elektraKeynameEscape(elektraKeyname(key)));

    ElektraKeyname newName;
    if (elektraKeynameUnescape("system:/foo/new", &newName) == 0) {
        elektraKeySetName(key, newName.name, newName.size);
    }

    ElektraKeyname * name = elektraKeyname(key);
    assert (strcmp(elektraKeynameLastPart(name), "extra") == 0);

    elektraKeynameAddPart(name, "extra");
    assert (strcmp(elektraKeynameLastPart(name), "extra") == 0);

    elektraKeynameAddEscaped(name, "even/more");

    assert (strcmp(elektraKeynameLastPart(name), "more") == 0);
    assert (strcmp(elektraKeynameEscape(name), "system:/foo/new/extra/even/more") == 0);
}
```

The main advantage of these APIs would be that they work both independently of a `Key` and with names on the stack.

### Private Buffer struct

Same as above, but the struct would be private and opaque.
Since the struct is so simple, this would have very few benefits.
However, it has a few downsides.

If the struct should be used in the public API and provided by the user, it would have to be separately allocated on the heap via a constructor.
Additionally, we'd need accessor functions to manipulate the data in a buffer struct.

### Keep current situation

Pointer to data and size as separate fields in structs.

This of course means, no struct for the public API and separate accessors for data pointers and data sizes.

## Decision

## Rationale

## Implications

## Related Decisions

- The buffer struct could be embedded in the various [Copy On Write](../1_in_discussion/copy_on_write.md) structures.
  By using a common struct, internal APIs for common task (e.g. resizing) can be created.

## Notes
