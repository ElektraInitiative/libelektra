package kdb_test

import (
	"errors"
	"testing"

	elektra "go.libelektra.org/src/bindings/go-elektra/kdb"
	. "go.libelektra.org/src/bindings/go-elektra/test"
)

func TestOpen(t *testing.T) {
	kdb := elektra.New()

	err := kdb.Open()
	defer kdb.Close()

	Checkf(t, err, "kdb.Open() failed: %v", err)
}

func TestSet(t *testing.T) {
	kdb := elektra.New()

	err := kdb.Open()
	defer kdb.Close()

	Checkf(t, err, "kdb.Open() failed: %v", err)

	ks := elektra.NewKeySet()
	parentKey, _ := elektra.NewKey("user:/tests/go/elektra/set")
	_, _ = kdb.Get(ks, parentKey)

	key, _ := elektra.NewKey("user:/tests/go/elektra/set/key")
	ks.AppendKey(key)

	_, err = kdb.Set(ks, parentKey)
	Checkf(t, err, "kdb Set failed %v", err)
}

func TestRemoveKey(t *testing.T) {
	kdb := elektra.New()
	namespace := "user:/tests/go/elektra/removekey"

	parentKey, err := elektra.NewKey(namespace)
	Check(t, err, "could not create parent Key")

	err = kdb.Open()
	Check(t, err, "could not open KDB")
	defer kdb.Close()

	k, err := elektra.NewKey(namespace+"/helloworld", "Hello World")
	Check(t, err, "could not create Key")

	k2, err := elektra.NewKey(namespace+"/helloworld2", "Hello World 2")
	Check(t, err, "could not create Key")

	ks := elektra.NewKeySet()
	Check(t, err, "could not create KeySet")

	changed, err := kdb.Get(ks, parentKey)
	Assert(t, changed, "kdb.Get() has not retrieved any keys")
	Check(t, err, "could not Get KeySet")

	ks.AppendKey(k)
	Check(t, err, "could not append Key to KeySet")

	ks.AppendKey(k2)
	Check(t, err, "could not append Key to KeySet")

	changed, err = kdb.Set(ks, parentKey)
	Assert(t, changed, "kdb.Set() has not updated any keys")
	Check(t, err, "could not Set KeySet")

	_, err = kdb.Get(ks, parentKey)
	Check(t, err, "could not Get KeySet")

	foundKey := ks.LookupByName("/tests/go/elektra/removekey/helloworld")
	Assertf(t, foundKey != nil, "KeySet does not contain key %s", k.Name())

	foundKey = ks.Lookup(k2)
	Assertf(t, foundKey != nil, "KeySet does not contain key %s", k2.Name())

	removed := ks.Remove(k2)
	Assert(t, removed != nil, "could not delete Key")

	changed, err = kdb.Set(ks, parentKey)
	Assert(t, changed, "kdb.Set() has not updated any keys")
	Check(t, err, "could not set KeySet")

	_, err = kdb.Get(ks, parentKey)
	Check(t, err, "could not Get KeySet")

	foundKey = ks.Lookup(k)
	Assertf(t, foundKey != nil, "KeySet does not contain key %s", k.Name())

	foundKey = ks.Lookup(k2)
	Assertf(t, foundKey == nil, "KeySet contains key %s", k2.Name())
}

func TestConflict(t *testing.T) {
	kdb1 := elektra.New()
	kdb2 := elektra.New()

	ks1 := elektra.NewKeySet()
	ks2 := elektra.NewKeySet()

	rootKey1, _ := elektra.NewKey("user:/tests/go/elektra/conflict")
	rootKey2, _ := elektra.NewKey("user:/tests/go/elektra/conflict")
	firstKey, _ := elektra.NewKey("user:/tests/go/elektra/conflict/first")
	secondKey, _ := elektra.NewKey("user:/tests/go/elektra/conflict/second")
	conflictKey, _ := elektra.NewKey("user:/tests/go/elektra/conflict/second")

	_ = kdb1.Open()
	defer kdb1.Close()

	_, _ = kdb1.Get(ks1, rootKey1)
	ks1.AppendKey(firstKey)
	_, _ = kdb1.Set(ks1, rootKey1)

	_ = kdb2.Open()
	defer kdb2.Close()
	_, _ = kdb2.Get(ks2, rootKey2)

	ks1.AppendKey(secondKey)
	_, _ = kdb1.Set(ks1, rootKey1)

	ks2.AppendKey(conflictKey)
	_, err := kdb2.Set(ks2, rootKey2)

	Assertf(t, errors.Is(err, elektra.ErrConflictingState), "expected conflict err: %v", err)
}

func TestVersion(t *testing.T) {
	kdb := elektra.New()

	err := kdb.Open()
	defer kdb.Close()

	version, err := kdb.Version()

	Checkf(t, err, "kdb.Version() failed: %v", err)
	Assert(t, version != "", "kdb.Version() is empty")
}
