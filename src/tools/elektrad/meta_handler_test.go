package main

import (
	"net/http"
	"testing"
)

func TestPostMeta(t *testing.T) {
	keyName := "user:/tests/elektrad/kdbmeta/post"
	value := "i'm a test value"
	meta := keyValueBody{
		Key:   "postmeta",
		Value: &value,
	}

	setupKeyWithMeta(t, keyName, meta)

	w := testPost(t, "/kdbMeta/"+keyName, keyValueBody{
		Key:   "postmeta",
		Value: &value,
	})

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusNoContent, "wrong status code: %v", code)

	key := getKey(t, keyName)
	Assert(t, key.Meta("postmeta") == value, "key has wrong meta value")
}

func TestDeleteMetaHandler(t *testing.T) {
	keyName := "user:/tests/elektrad/kdbmeta/delete/test"
	value := "value"
	meta := keyValueBody{
		Key:   "postmeta",
		Value: &value,
	}

	setupKeyWithMeta(t, keyName, meta)

	w := testDelete(t, "/kdbMeta/"+keyName, meta)

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusNoContent, "wrong status code: %v", code)

	metaValue := getKey(t, keyName).Meta("postmeta")
	Assertf(t, metaValue == "", "key meta value is not empty: %q", metaValue)
}
