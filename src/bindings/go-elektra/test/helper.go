package test

import (
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
