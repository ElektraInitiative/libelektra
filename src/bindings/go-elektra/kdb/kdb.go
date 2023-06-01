package kdb

// #cgo pkg-config: elektra
// #include <elektra/kdb.h>
import "C"

import (
	"errors"
)

// KDB (key data base) access functions
type KDB interface {
	Open() error
	Close() error

	Get(keySet KeySet, parentKey Key) (changed bool, err error)
	Set(keySet KeySet, parentKey Key) (changed bool, err error)

	Version() (string, error)
}

type KdbC struct {
	handle *C.struct__KDB
}

// New returns a new KDB instance.
func New() KDB {
	return &KdbC{}
}

// Open creates a handle to the kdb library,
// this is mandatory to Get / Set Keys.
func (e *KdbC) Open() error {
	key, err := newKey("/")

	if err != nil {
		return err
	}

	handle := C.kdbOpen(nil, key.Ptr)

	if handle == nil {
		return errFromKey(key)
	}

	e.handle = handle

	return nil
}

// Open creates a handle to the kdb library,
// this is mandatory to Get / Set Keys.
// This function also enforces a contract.
func (e *KdbC) OpenWithContract(contract KeySet) error {
	key, err := newKey("/")

	if err != nil {
		return err
	}

	cContract, err := toCKeySet(contract)

	handle := C.kdbOpen(cContract.Ptr, key.Ptr)

	if handle == nil {
		return errFromKey(key)
	}

	e.handle = handle

	return nil
}

// Close closes the kdb handle.
func (e *KdbC) Close() error {
	key, err := newKey("/")

	if err != nil {
		return err
	}

	ret := C.kdbClose(e.handle, key.Ptr)

	if ret < 0 {
		return errors.New("could not close kdb handle")
	}

	return nil
}

// Get retrieves parentKey and all Keys beneath it.
// Returns true if Keys have been loaded or updated and an
// error if something went wrong.
func (e *KdbC) Get(keySet KeySet, parentKey Key) (bool, error) {
	cKey, err := toCKey(parentKey)

	if err != nil {
		return false, err
	}

	cKeySet, err := toCKeySet(keySet)

	if err != nil {
		return false, err
	}

	changed := C.kdbGet(e.handle, cKeySet.Ptr, cKey.Ptr)

	if changed == -1 {
		return false, errFromKey(cKey)
	}

	return changed == 1, nil
}

// Set sets all Keys of a KeySet.
// Returns true if any of the keys have changed and an error if
// something happened (such as a conflict).
func (e *KdbC) Set(keySet KeySet, parentKey Key) (bool, error) {
	cKey, err := toCKey(parentKey)

	if err != nil {
		return false, err
	}

	cKeySet, err := toCKeySet(keySet)

	if err != nil {
		return false, err
	}

	changed := C.kdbSet(e.handle, cKeySet.Ptr, cKey.Ptr)

	if changed == -1 {
		return false, errFromKey(cKey)
	}

	return changed == 1, nil
}

// Version `Get`s the current version of Elektra from
// the "system:/elektra/version/constants/KDB_VERSION" key
// in the format Major.Minor.Micro, be aware that this can
// lead to unexpected state-changes.
func (e *KdbC) Version() (string, error) {
	k, err := NewKey("system:/elektra/version")

	if err != nil {
		return "", err
	}

	ks := NewKeySet()

	_, err = e.Get(ks, k)

	versionKey := ks.LookupByName("system:/elektra/version/constants/KDB_VERSION")
	version := versionKey.String()

	return version, nil
}
