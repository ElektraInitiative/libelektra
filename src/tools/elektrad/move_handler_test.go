package main

import (
	"net/http"
	"testing"
)

func TestPostMove(t *testing.T) {
	keyNameFrom := "user:/tests/elektrad/kdbmv/post/from"
	keyNameTo := "user:/tests/elektrad/kdbmv/post/to"

	setupKey(t, keyNameFrom)
	removeKey(t, keyNameTo)

	w := testPost(t, "/kdbMv/"+keyNameFrom, keyNameTo)

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusNoContent, "wrong status code: %v", code)

	key := getKey(t, keyNameTo)
	removeKey(t, keyNameTo)
	Assert(t, key != nil, "key has not been moved")
}
