package kdb_test

import (
	"bytes"
	"fmt"
	"math/rand"
	"testing"

	elektra "go.libelektra.org/kdb"
	. "go.libelektra.org/test"
)

func TestName(t *testing.T) {
	firstName := "user:/tests/go/elektra/name/1"
	k, err := elektra.NewKey(firstName)

	Check(t, err, "could not create key")
	Assert(t, k.Name() == firstName, "wrong key name")

	secondName := "user:/tests/go/elektra/name/2"
	err = k.SetName(secondName)

	Check(t, err, "could not set key name")
	Assert(t, k.Name() == secondName, "could not set name")
}

func TestString(t *testing.T) {
	testValue := "Hello World"

	k, err := elektra.NewKey("user:/tests/go/elektra/string", testValue)
	Check(t, err, "could not create key")

	val := k.String()
	Assertf(t, val == testValue, "Key.GetString() did not match %q", testValue)
}

var bytesTests = []struct {
	keyName  string
	key2     string
	expected string
}{
	{"user:/foo/bar", "user:/foo/bar2", "user:/foo"},
	{"proc:/foo/bar", "user:/foo/bar", "/foo/bar"},
	{"user:/foo/bar", "user:/bar/foo", "user:/"},
	{"proc:/bar/foo", "user:/foo/bar", ""},
}

func TestBytes(t *testing.T) {
	values := [][]byte{
		make([]byte, 10),
		make([]byte, 0),
		[]byte("Test SetBytes"),
	}

	rand.Read(values[0])

	for testcase, want := range values {
		k, err := elektra.NewKey("user:/tests/go/elektra/bytes")
		Check(t, err, "could not create key")

		err = k.SetBytes(want)
		Check(t, err, "SetBytes failed")

		got := k.Bytes()
		Assertf(t, bytes.Compare(got, want) == 0, "Testcase %d: Key.Bytes() %X did not match %X", testcase, got, want)
	}
}

func TestMeta(t *testing.T) {
	k, err := elektra.NewKey("user:/tests/go/elektra/meta", "Hello World")
	Check(t, err, "could not create key")

	err = k.SetMeta("meta", "value")
	Check(t, err, "could not set meta")

	val := k.Meta("meta")
	Assert(t, val == "value", "Key.Meta() did not return the correct value")
}

func keyWithMetaKeys(t *testing.T, name string, keyValues map[string]string) elektra.Key {
	t.Helper()

	key, err := elektra.NewKey(name)
	Check(t, err, "could not create key")

	for metaName, value := range keyValues {
		err := key.SetMeta(metaName, value)
		Checkf(t, err, "could not set meta value: %v", err)
	}

	return key
}

func TestMetaMap(t *testing.T) {
	keyValues := map[string]string{
		"foo": "foo",
		"bar": "bar",
		"baz": "baz",
	}

	key := keyWithMetaKeys(t, "user:/tests/go/elektra/meta", keyValues)

	metaMap := key.MetaMap()

	for k, v := range keyValues {
		metaKey, ok := metaMap[k]
		Assertf(t, ok, "MetaMap() does not contain the Key %q", k)
		Assert(t, metaKey == v, "MetaMap contans the wrong Value")
	}

	Assertf(t, len(metaMap) == len(keyValues), "Len(MetaSlice()) is unexpected, got: %d, want: %d", len(metaMap), len(keyValues))
}

func TestMetaSlice(t *testing.T) {
	keyValues := map[string]string{
		"meta:/foo": "foo",
		"meta:/bar": "bar",
		"meta:/baz": "baz",
	}

	key := keyWithMetaKeys(t, "user:/tests/go/elektra/meta", keyValues)

	metaSlice := key.MetaSlice()

	for _, metaKey := range metaSlice {
		value, ok := keyValues[metaKey.Name()]
		Assert(t, ok, "MetaSlice() contains wrong Key")
		Assert(t, value == metaKey.String(), "MetaSlice returns Key with wrong Value")
	}

	Assertf(t, len(metaSlice) == len(keyValues), "Len(MetaSlice()) is unexpected, got: %d, want: %d", len(metaSlice), len(keyValues))
}

func TestNamespace(t *testing.T) {
	key, _ := elektra.NewKey("user:/tests/go/elektra/namespace")

	expected := elektra.KEY_NS_USER
	namespace := key.Namespace()
	Assertf(t, namespace == expected, "Namespace be %q but is %q", expected, namespace)

	key, _ = elektra.NewKey("/go-elektra/namespace")

	expected = elektra.KEY_NS_CASCADING
	namespace = key.Namespace()
	Assertf(t, namespace == expected, "Namespace be %q but is %q", expected, namespace)
}

var commonKeyNameTests = []struct {
	key1     string
	key2     string
	expected string
}{
	{"user:/foo/bar", "user:/foo/bar2", "user:/foo"},
	{"proc:/foo/bar", "user:/foo/bar", "/foo/bar"},
	{"user:/foo/bar", "user:/bar/foo", "user:/"},
	{"proc:/bar/foo", "user:/foo/bar", ""},
}

func TestCommonKeyName(t *testing.T) {
	for _, test := range commonKeyNameTests {
		t.Run(fmt.Sprintf("(%q, %q)", test.key1, test.key2), func(t *testing.T) {
			key1, _ := elektra.NewKey(test.key1)
			key2, _ := elektra.NewKey(test.key2)

			commonName := elektra.CommonKeyName(key1, key2)
			Assertf(t, commonName == test.expected, "commonName should be %q but is %q", test.expected, commonName)
		})
	}
}
