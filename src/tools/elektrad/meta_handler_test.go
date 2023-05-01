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
	removeKey(t, keyName)
	Assert(t, key.Meta("postmeta") == value, "key has wrong meta value")
}

func TestPostMetaBulk(t *testing.T) {
	keyName := "user:/tests/elektrad/kdbmetabulk/post"
	value := "Bulk set meta keys test value"

	metaOne := keyValueBody{
		Key:   "postmetabulkone",
		Value: &value,
	}
	metaTwo := keyValueBody{
		Key:   "postmetabulktwo",
		Value: &value,
	}

	setupKey(t, keyName)

	metaSet := []keyValueBody{metaOne, metaTwo}

	w := testPost(t, "/kdbMetaBulk/"+keyName, metaKeySet{metaSet})
	code := w.Result().StatusCode
	Assertf(t, code == http.StatusNoContent, "wrong status code: %v", code)

	containsMeta(t, keyName, metaSet)
}

func TestDeleteMetaHandler(t *testing.T) {
	keyName := "user:/tests/elektrad/kdbmeta/delete/test"
	value := "value"
	meta := keyValueBody{
		Key:   "delmeta",
		Value: &value,
	}

	setupKeyWithMeta(t, keyName, meta)

	w := testDelete(t, "/kdbMeta/"+keyName, meta)

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusNoContent, "wrong status code: %v", code)

	key := getKey(t, keyName)
	removeKey(t, keyName)
	Assert(t, key != nil, "key not found")

	metaValue := key.Meta(meta.Key)
	Assertf(t, metaValue == "", "key meta value is not empty: %q", metaValue)
}
