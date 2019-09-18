package main

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

func testGet(t *testing.T, path string) *httptest.ResponseRecorder {
	t.Helper()

	return testRequest(t, "GET", path, nil)
}

func testDelete(t *testing.T, path string, body interface{}) *httptest.ResponseRecorder {
	t.Helper()

	return testRequest(t, "DELETE", path, body)
}

func testPost(t *testing.T, path string, body interface{}) *httptest.ResponseRecorder {
	t.Helper()

	return testRequest(t, "POST", path, body)
}

func setupKeyWithMeta(t *testing.T, keyName string, meta ...keyValueBody) {
	t.Helper()

	parentKey, err := elektra.CreateKey(keyName)
	Checkf(t, err, "could not create key %s: %v", keyName, err)

	kdb := elektra.New()
	err = kdb.Open(parentKey)
	Checkf(t, err, "could not open kdb: %v", err)

	ks, err := getKeySet(kdb, parentKey)
	Checkf(t, err, "could not get KeySet: %v", err)

	k, err := ks.Lookup(parentKey)
	Checkf(t, err, "could not get key %s: %v", keyName, err)

	if k == nil {
		err = ks.AppendKey(k)
		Checkf(t, err, "could not append key: %v", err)
	}

	for _, m := range meta {
		err = k.SetMeta(m.Key, *m.Value)
		Checkf(t, err, "could not append key: %v", err)
	}
}

func testRequest(t *testing.T, verb, path string, body interface{}) *httptest.ResponseRecorder {
	t.Helper()

	var jsonBody io.Reader

	if body != nil {
		marshalled, err := json.Marshal(body)

		Checkf(t, err, "unmarshal %s request: %v", verb, err)

		jsonBody = bytes.NewReader(marshalled)
	}

	r := setupRouter()

	w := httptest.NewRecorder()

	req, err := http.NewRequest(verb, path, jsonBody)

	Checkf(t, err, "could not create %s request: %v", verb, err)

	r.ServeHTTP(w, req)

	return w
}

func getKey(t *testing.T, keyName string) elektra.Key {
	t.Helper()

	parentKey, err := elektra.CreateKey(keyName)
	Checkf(t, err, "could not create key %s: %v", keyName, err)

	kdb := elektra.New()
	err = kdb.Open(parentKey)
	Checkf(t, err, "could not open kdb: %v", err)

	ks, err := getKeySet(kdb, parentKey)
	Checkf(t, err, "could not get KeySet: %v", err)

	k, err := ks.Lookup(parentKey)
	Checkf(t, err, "could not get key %s: %v", keyName, err)
	Assertf(t, k != nil, "key %q does not exist", keyName)

	return k
}
