package kdb

// #include <kdb.h>
// #include <stdlib.h>
//
// static KeySet * ksNewWrapper(size_t size) {
// 	 return ksNew(size, KEY_END);
// }
import "C"

import (
	"unsafe"

	"errors"
)

// KeySet represents a collection of Keys.
type KeySet interface {
	Copy(keySet KeySet)
	Append(keySet KeySet) int
	AppendKey(key Key) int
	Remove(key Key) Key
	RemoveByName(name string) Key
	Duplicate() KeySet

	Pop() Key
	Len() int

	Cut(key Key) KeySet

	Close()

	ForEach(iterator Iterator)
	ToSlice() []Key
	KeyNames() []string

	Clear()

	Lookup(key Key) Key
	LookupByName(name string) Key
}

type CKeySet struct {
	Ptr *C.struct__KeySet
}

// NewKeySet creates a new KeySet.
func NewKeySet(keys ...Key) KeySet {
	size := len(keys)
	ks := wrapKeySet(C.ksNewWrapper(C.ulong(size)))

	for _, k := range keys {
		ks.AppendKey(k)
	}

	return ks
}

func wrapKeySet(ks *C.struct__KeySet) *CKeySet {
	if ks == nil {
		return nil
	}

	keySet := &CKeySet{Ptr: ks}

	return keySet
}

// Close needs to be called on a KeySet after it is not in
// use anymore to free the allocated memory.
func (ks *CKeySet) Close() {
	C.ksDel(ks.Ptr)
}

func toCKeySet(keySet KeySet) (*CKeySet, error) {
	if keySet == nil {
		return nil, errors.New("keyset is nil")
	}

	ckeySet, ok := keySet.(*CKeySet)

	if !ok {
		return nil, errors.New("only instances of KeySet that were created by elektra/kdb may be passed to this function")
	}

	return ckeySet, nil
}

// Append appends all Keys from `other` to this KeySet and returns the
// new length of this KeySet or -1 if `other` is not a KeySet which was
// created by elektra/kdb.
func (ks *CKeySet) Append(other KeySet) int {
	ckeySet, err := toCKeySet(other)

	if err != nil {
		return -1
	}

	ret := int(C.ksAppend(ks.Ptr, ckeySet.Ptr))

	return ret
}

// Duplicate returns a new duplicated keyset.
func (ks *CKeySet) Duplicate() KeySet {
	return wrapKeySet(C.ksDup(ks.Ptr))
}

// AppendKey appends a Key to this KeySet and returns the new
// length of this KeySet or -1 if the key is
// not a Key created by elektra/kdb.
func (ks *CKeySet) AppendKey(key Key) int {
	ckey, err := toCKey(key)

	if err != nil {
		return -1
	}

	size := int(C.ksAppendKey(ks.Ptr, ckey.Ptr))

	return size
}

// Cut cuts out a new KeySet at the cutpoint key and returns it.
func (ks *CKeySet) Cut(key Key) KeySet {
	k, err := toCKey(key)

	if err != nil {
		return nil
	}

	newKs := wrapKeySet(C.ksCut(ks.Ptr, k.Ptr))

	return newKs
}

// ToSlice returns a slice containing all Keys.
func (ks *CKeySet) ToSlice() []Key {
	var keys = make([]Key, ks.Len())

	ks.forEach(func(k Key, i int) {
		keys[i] = k
	})

	return keys
}

// Iterator is a function that loops over Keys.
type Iterator func(k Key, i int)

// toKey returns a cached Key that wraps the *C.struct__Key -
// or creates a new wrapped *CKey.
func (ks *CKeySet) toKey(k *C.struct__Key) *CKey {
	if k == nil {
		return nil
	}

	return wrapKey(k)
}

// forEach provides an easy way of looping of the keyset by passing
// an iterator function.
func (ks *CKeySet) forEach(iterator Iterator) {
	cursor := C.elektraCursor(0)

	if ks.Len() < 1 {
		return
	}

	next := func() Key {
		key := ks.toKey(C.ksAtCursor(ks.Ptr, cursor))
		cursor++

		if key == nil {
			return nil
		}

		return key
	}

	for key := next(); key != nil; key = next() {
		iterator(key, int(cursor)-1)
	}
}

// ForEach accepts an `Iterator` that loops over every Key in the KeySet.
func (ks *CKeySet) ForEach(iterator Iterator) {
	ks.forEach(iterator)
}

// KeyNames returns a slice of the name of every Key in the KeySet.
func (ks *CKeySet) KeyNames() []string {
	var keys = make([]string, ks.Len())

	ks.forEach(func(k Key, i int) {
		keys[i] = k.Name()
	})

	return keys
}

// Copy copies the entire KeySet to the passed KeySet.
func (ks *CKeySet) Copy(keySet KeySet) {
	cKeySet, err := toCKeySet(keySet)

	if err != nil {
		return
	}

	C.ksCopy(cKeySet.Ptr, ks.Ptr)

	return
}

// Pop removes and returns the last Element that was added to the KeySet.
func (ks *CKeySet) Pop() Key {
	key := C.ksPop(ks.Ptr)

	return wrapKey(key)
}

// Remove removes a key from the KeySet and returns it if found.
func (ks *CKeySet) Remove(key Key) Key {
	ckey, err := toCKey(key)

	if err != nil {
		return nil
	}

	removed := C.ksLookup(ks.Ptr, ckey.Ptr, C.KDB_O_POP)

	return wrapKey(removed)
}

// RemoveByName removes a key by its name from the KeySet and returns it if found.
func (ks *CKeySet) RemoveByName(name string) Key {
	n := C.CString(name)
	defer C.free(unsafe.Pointer(n))

	key := C.ksLookupByName(ks.Ptr, n, C.KDB_O_POP)

	return wrapKey(key)
}

// Clear removes all Keys from the KeySet.
func (ks *CKeySet) Clear() {
	root, _ := newKey("/")

	// don't use `ksClear` because it is internal
	// and renders the KeySet unusable
	newKs := C.ksCut(ks.Ptr, root.Ptr)

	// we don't need this keyset
	C.ksDel(newKs)
}

// Lookup searches the KeySet for a certain Key.
func (ks *CKeySet) Lookup(key Key) Key {
	ckey, err := toCKey(key)

	if err != nil {
		return nil
	}

	if foundKey := ks.toKey(C.ksLookup(ks.Ptr, ckey.Ptr, 0)); foundKey != nil {
		return foundKey
	}

	return nil
}

// LookupByName searches the KeySet for a Key by name.
func (ks *CKeySet) LookupByName(name string) Key {
	n := C.CString(name)
	defer C.free(unsafe.Pointer(n))

	if key := ks.toKey(C.ksLookupByName(ks.Ptr, n, 0)); key != nil {
		return key
	}

	return nil
}

// Len returns the length of the KeySet.
func (ks *CKeySet) Len() int {
	return int(C.ksGetSize(ks.Ptr))
}

/*****
	The following functions are for benchmarks only
	and should not be exported
*****/

func (ks *CKeySet) toSliceWithoutInitialization() []Key {
	var keys = []Key{}

	ks.forEach(func(k Key, i int) {
		keys = append(keys, k)
	})

	return keys
}
