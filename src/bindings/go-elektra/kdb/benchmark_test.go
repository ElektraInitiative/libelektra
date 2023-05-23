package kdb

import (
	"fmt"
	"testing"

	. "github.com/ElektraInitiative/libelektra/src/bindings/go-elektra/test"
)

func setupTestData(b *testing.B, count int) *CKeySet {
	b.Helper()

	keys := make([]Key, count)

	for n := 0; n < count; n++ {
		k, err := NewKey(fmt.Sprintf("proc:/tests/go/elektra/benchmark/iterator/callback/%08d", n))
		Checkf(b, err, "kdb.NewKey() failed: %v", err)

		keys[n] = k
	}

	ks := NewKeySet(keys...)

	b.ResetTimer()
	return ks.(*CKeySet)
}

const dataSize = 100000

func BenchmarkKeySetExternalCallbackIterator(b *testing.B) {
	ks := setupTestData(b, dataSize)

	for n := 0; n < b.N; n++ {
		ks.ForEach(func(k Key, i int) {
		})
	}
}

func BenchmarkKeySetSliceRangeIterator(b *testing.B) {
	ks := setupTestData(b, dataSize)

	for n := 0; n < b.N; n++ {
		ksSlice := ks.ToSlice()

		for range ksSlice {
		}
	}
}

func BenchmarkKeySetSlowerSliceRangeIterator(b *testing.B) {
	ks := setupTestData(b, dataSize)
	defer ks.Close()

	for n := 0; n < b.N; n++ {
		ksSlice := ks.toSliceWithoutInitialization()

		for range ksSlice {
		}
	}
}
