package main

import (
	"sort"
	"testing"
)

func Assert(t testing.TB, assertion bool, message string) {
	t.Helper()

	Assertf(t, assertion, message)
}

func Assertf(t testing.TB, assertion bool, message string, args ...interface{}) {
	t.Helper()

	if assertion {
		return
	}

	t.Fatalf(message, args...)
}

func Check(t testing.TB, err error, message string) {
	t.Helper()

	Checkf(t, err, "%s: %v", message, err)
}

func Checkf(t testing.TB, err error, message string, args ...interface{}) {
	t.Helper()

	if err == nil {
		return
	}

	t.Fatalf(message, args...)
}

func CompareStrings(t testing.TB, slice1, slice2 []string, msg string) {
	t.Helper()

	if len(slice1) != len(slice2) {
		t.Fatalf("%s, slices are not of equal length %d and %d", msg, len(slice1), len(slice2))
	}
	sort.Strings(slice1)
	sort.Strings(slice2)

	for i := 0; i < len(slice1); i++ {
		if slice1[i] != slice2[i] {
			t.Errorf("%s [%d] %s != %s", msg, i, slice1[i], slice2[i])
		}
	}
}
