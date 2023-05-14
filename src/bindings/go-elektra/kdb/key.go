package kdb

// #include <kdb.h>
// #include <stdlib.h>
//
//
// static Key * keyNewWrapper(char* k) {
//   return keyNew(k, KEY_END);
// }
//
// static Key * keyNewValueWrapper(char* k, char* v) {
//   return keyNew(k, KEY_VALUE, v, KEY_END);
// }
import "C"

import (
	"errors"
	"strings"
	"unsafe"
)

type ElektraNamespace uint

const (
	KEY_NS_NONE      ElektraNamespace = C.KEY_NS_NONE
	KEY_NS_CASCADING ElektraNamespace = C.KEY_NS_CASCADING
	KEY_NS_META      ElektraNamespace = C.KEY_NS_META
	KEY_NS_SPEC      ElektraNamespace = C.KEY_NS_SPEC
	KEY_NS_PROC      ElektraNamespace = C.KEY_NS_PROC
	KEY_NS_DIR       ElektraNamespace = C.KEY_NS_DIR
	KEY_NS_USER      ElektraNamespace = C.KEY_NS_USER
	KEY_NS_SYSTEM    ElektraNamespace = C.KEY_NS_SYSTEM
	KEY_NS_DEFAULT   ElektraNamespace = C.KEY_NS_DEFAULT
)

type KeyCopyFlags uint

const (
	KEY_CP_NAME   KeyCopyFlags = C.KEY_CP_NAME
	KEY_CP_VALUE  KeyCopyFlags = C.KEY_CP_VALUE
	KEY_CP_STRING KeyCopyFlags = C.KEY_CP_STRING
	KEY_CP_META   KeyCopyFlags = C.KEY_CP_META
	KEY_CP_ALL    KeyCopyFlags = C.KEY_CP_ALL
)

// Key is the wrapper around the Elektra Key.
type Key interface {
	Name() string
	Namespace() ElektraNamespace
	BaseName() string

	String() string
	Bytes() []byte

	Close()

	Meta(name string) string
	MetaMap() map[string]string
	RemoveMeta(name string) error
	MetaSlice() []Key

	IsBelow(key Key) bool
	IsBelowOrSame(key Key) bool
	IsDirectlyBelow(key Key) bool
	Compare(key Key) int

	Duplicate(flags KeyCopyFlags) Key

	SetMeta(name, value string) error
	SetName(name string) error
	SetString(value string) error
	SetBytes(value []byte) error
}

type CKey struct {
	Ptr *C.struct__Key
}

// NewKey creates a new `Key` with an optional value.
func NewKey(name string, value ...interface{}) (Key, error) {
	return newKey(name, value...)
}

// newKey is not exported and should only be used internally in this package because the C pointer should not be exposed to packages using these bindings
// Its useful since the C pointer can be used directly without having to cast from `Key` first.
func newKey(name string, value ...interface{}) (*CKey, error) {
	var key *CKey

	n := C.CString(name)
	defer C.free(unsafe.Pointer(n))

	if name == "" {
		return nil, errors.New("unsupported key name")
	} else if len(value) > 0 {
		switch v := value[0].(type) {
		case string:
			cValue := C.CString(v)
			key = wrapKey(C.keyNewValueWrapper(n, cValue))
			defer C.free(unsafe.Pointer(cValue))
		default:
			return nil, errors.New("unsupported key value type")
		}
	} else {
		key = wrapKey(C.keyNewWrapper(n))
	}

	if key == nil {
		return nil, errors.New("could not create key (check the key name)")
	}

	return key, nil
}

func wrapKey(k *C.struct__Key) *CKey {
	if k == nil {
		return nil
	}

	key := &CKey{Ptr: k}

	return key
}

// Close free's the underlying key's memory. This needs to be done
// for Keys that are created by NewKey() or Key.Duplicate().
func (k *CKey) Close() {
	C.keyDel(k.Ptr)
}

func toCKey(key Key) (*CKey, error) {
	if key == nil {
		return nil, errors.New("key is nil")
	}

	CKey, ok := key.(*CKey)

	if !ok {
		return nil, errors.New("only pointer to CKey struct allowed")
	}

	return CKey, nil
}

// BaseName returns the basename of the Key.
// Some examples:
// - BaseName of system:/some/keyname is keyname
// - BaseName of "user:/tmp/some key" is "some key"
func (k *CKey) BaseName() string {
	name := C.keyBaseName(k.Ptr)

	return C.GoString(name)
}

// Name returns the name of the Key.
func (k *CKey) Name() string {
	name := C.keyName(k.Ptr)

	return C.GoString(name)
}

// SetBytes sets the value of a key to a byte slice.
func (k *CKey) SetBytes(value []byte) error {
	v := C.CBytes(value)
	defer C.free(unsafe.Pointer(v))

	size := C.ulong(len(value))

	C.keySetBinary(k.Ptr, unsafe.Pointer(v), size)

	return nil
}

// SetString sets the string of a key.
func (k *CKey) SetString(value string) error {
	v := C.CString(value)
	defer C.free(unsafe.Pointer(v))

	_ = C.keySetString(k.Ptr, v)

	return nil
}

// SetBoolean sets the string of a key to a boolean
// where true is represented as "1" and false as "0".
func (k *CKey) SetBoolean(value bool) error {
	strValue := "0"

	if value {
		strValue = "1"
	}

	return k.SetString(strValue)
}

// SetName sets the name of the Key.
func (k *CKey) SetName(name string) error {
	n := C.CString(name)
	defer C.free(unsafe.Pointer(n))

	if ret := C.keySetName(k.Ptr, n); ret < 0 {
		return errors.New("could not set key name")
	}

	return nil
}

// Bytes returns the value of the Key as a byte slice.
func (k *CKey) Bytes() []byte {
	size := (C.ulong)(C.keyGetValueSize(k.Ptr))

	buffer := unsafe.Pointer((*C.char)(C.malloc(size)))
	defer C.free(buffer)

	ret := C.keyGetBinary(k.Ptr, buffer, C.ulong(size))

	if ret <= 0 {
		return []byte{}
	}

	bytes := C.GoBytes(buffer, C.int(size))

	return bytes
}

// String returns the string value of the Key.
func (k *CKey) String() string {
	str := C.keyString(k.Ptr)

	return C.GoString(str)
}

// SetMeta sets the meta value of a Key.
func (k *CKey) SetMeta(name, value string) error {
	cName, cValue := C.CString(name), C.CString(value)

	defer C.free(unsafe.Pointer(cName))
	defer C.free(unsafe.Pointer(cValue))

	ret := C.keySetMeta(k.Ptr, cName, cValue)

	if ret < 0 {
		return errors.New("could not set meta")
	}

	return nil
}

// RemoveMeta deletes a meta Key.
func (k *CKey) RemoveMeta(name string) error {
	cName := C.CString(name)

	defer C.free(unsafe.Pointer(cName))

	ret := C.keySetMeta(k.Ptr, cName, nil)

	if ret < 0 {
		return errors.New("could not delete meta")
	}

	return nil
}

// Meta retrieves the Meta value of a Key.
func (k *CKey) Meta(name string) string {
	cName := C.CString(name)

	defer C.free(unsafe.Pointer(cName))

	metaKey := wrapKey(C.keyGetMeta(k.Ptr, cName))

	if metaKey == nil {
		return ""
	}

	return metaKey.String()
}

// MetaSlice builds a slice of all meta Keys.
func (k *CKey) MetaSlice() []Key {
	metaKs := C.keyMeta(k.Ptr)
	var metaKeys []Key
	for it := C.long(0); it < C.ksGetSize(metaKs); it++ {
		metaKeys = append(metaKeys, wrapKey(C.ksAtCursor(metaKs, it)))
	}

	return metaKeys
}

// MetaMap builds a Key/Value map of all meta Keys.
func (k *CKey) MetaMap() map[string]string {

	metaKs := C.keyMeta(k.Ptr)
	m := make(map[string]string)

	for it := C.long(0); it < C.ksGetSize(metaKs); it++ {
		curMeta := wrapKey(C.ksAtCursor(metaKs, it))
		m[strings.TrimPrefix(curMeta.Name(), "meta:/")] = curMeta.String()
	}

	return m
}

// Duplicate duplicates a Key.
func (k *CKey) Duplicate(flags KeyCopyFlags) Key {
	return wrapKey(C.keyDup(k.Ptr, C.uint(flags)))
}

// IsBelow checks if this key is below the `other` key.
func (k *CKey) IsBelow(other Key) bool {
	otherKey, err := toCKey(other)

	if err != nil {
		return false
	}

	ret := C.keyIsBelow(otherKey.Ptr, k.Ptr)

	return ret != 0
}

// IsBelowOrSame checks if this key is below or the same as the `other` key.
func (k *CKey) IsBelowOrSame(other Key) bool {
	otherKey, err := toCKey(other)

	if err != nil {
		return false
	}

	ret := C.keyIsBelowOrSame(otherKey.Ptr, k.Ptr)

	return ret != 0
}

// IsDirectlyBelow checks if this key is directly below the `other` Key.
func (k *CKey) IsDirectlyBelow(other Key) bool {
	otherKey, err := toCKey(other)

	if err != nil {
		return false
	}

	ret := C.keyIsDirectlyBelow(otherKey.Ptr, k.Ptr)

	return ret != 0
}

// Compare the name of two keys. It returns 0 if the keys are equal,
// < 0 if this key is less than `other` Key and
// > 0 if this key is greater than `other` Key.
// This function defines the sorting order of a KeySet.
func (k *CKey) Compare(other Key) int {
	otherKey, _ := toCKey(other)

	return int(C.keyCmp(k.Ptr, otherKey.Ptr))
}

// Namespace returns the namespace of a Key.
func (k *CKey) Namespace() ElektraNamespace {
	return ElektraNamespace(C.keyGetNamespace(k.Ptr))
}

func nameWithoutNamespace(key Key) string {
	name := key.Name()
	index := strings.Index(name, "/")

	if index < 0 {
		return "/"
	}

	return name[index:]
}

// CommonKeyName returns the common path of two Keys.
func CommonKeyName(key1, key2 Key) string {
	key1Name := key1.Name()
	key2Name := key2.Name()

	if key1.IsBelowOrSame(key2) {
		return key2Name
	}
	if key2.IsBelowOrSame(key1) {
		return key1Name
	}

	key1Path := nameWithoutNamespace(key1)
	key2Path := nameWithoutNamespace(key2)

	ns := "/"
	if key1.Namespace() == key2.Namespace() {
		ns = key1Name[:strings.Index(key1Name, "/")] + "/"
	} else if key1Path[2] != key2Path[2] {
		return ""
	}

	index := 0
	k1Parts, k2Parts := strings.Split(key1Path[1:], "/"), strings.Split(key2Path[1:], "/")

	for ; index < len(k1Parts) && index < len(k2Parts) && k1Parts[index] == k2Parts[index]; index++ {
	}

	return ns + strings.Join(k1Parts[:index], "/")
}
