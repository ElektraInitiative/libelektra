package main

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	elektra "go.libelektra.org/kdb"
)

func testGet(t *testing.T, path string) *httptest.ResponseRecorder {
	t.Helper()

	return testRequest(t, "GET", path, nil)
}

func testDelete(t *testing.T, path string, body interface{}) *httptest.ResponseRecorder {
	t.Helper()

	return testRequest(t, "DELETE", path, body)
}

func testPut(t *testing.T, path string, body interface{}) *httptest.ResponseRecorder {
	t.Helper()

	return testRequest(t, "PUT", path, body)
}

func testPost(t *testing.T, path string, body interface{}) *httptest.ResponseRecorder {
	t.Helper()

	return testRequest(t, "POST", path, body)
}

func parseBody(t *testing.T, w *httptest.ResponseRecorder, result interface{}) {
	t.Helper()

	decoder := json.NewDecoder(w.Body)

	err := decoder.Decode(&result)
	Checkf(t, err, "unable to unmarshal the response body")
}

func setupKey(t *testing.T, keyNames ...string) {
	t.Helper()

	rootKey, err := elektra.NewKey("/")
	Checkf(t, err, "could not create key: %v", err)

	kdb := elektra.New()
	err = kdb.Open()
	Checkf(t, err, "could not open kdb: %v", err)

	ks := elektra.NewKeySet()
	_, err = kdb.Get(ks, rootKey)
	Checkf(t, err, "could not get KeySet: %v", err)

	for _, k := range keyNames {
		parentKey, err := elektra.NewKey(k)
		Checkf(t, err, "could not create key %s: %v", k, err)

		key := ks.Lookup(parentKey)
		Checkf(t, err, "could not get key %s: %v", k, err)

		if key == nil {
			ks.AppendKey(parentKey)
			Checkf(t, err, "could not append key: %v", err)
		}
	}

	_, err = kdb.Set(ks, rootKey)
	Checkf(t, err, "could not save keys %+v: %v", keyNames, err)
}

func removeKey(t *testing.T, keyName string) {
	t.Helper()

	errKey, err := elektra.NewKey(keyName)
	Checkf(t, err, "could not create key: %v", err)

	parentKey, err := elektra.NewKey(keyName)
	Checkf(t, err, "could not create key: %v", err)

	kdb := elektra.New()
	err = kdb.Open()
	Checkf(t, err, "could not open kdb: %v", err)

	ks := elektra.NewKeySet()
	_, err = kdb.Get(ks, errKey)
	Checkf(t, err, "could not create key: %v", err)

	k := ks.Lookup(parentKey)
	Checkf(t, err, "could not get key %s: %v", keyName, err)

	if k == nil {
		return
	}

	ks.Remove(k)
	Checkf(t, err, "could not remove key %s: %v", keyName, err)

	_, err = kdb.Set(ks, errKey)
	Checkf(t, err, "could not save removal of key %s: %v", keyName, err)
}

func setupKeyWithMeta(t *testing.T, keyName string, meta ...keyValueBody) {
	t.Helper()

	errKey, err := elektra.NewKey(keyName)
	Checkf(t, err, "could not create key: %v", err)

	parentKey, err := elektra.NewKey(keyName)
	Checkf(t, err, "could not create key: %v", err)

	kdb := elektra.New()
	err = kdb.Open()
	Checkf(t, err, "could not open kdb: %v", err)

	ks := elektra.NewKeySet()
	_, err = kdb.Get(ks, errKey)
	Checkf(t, err, "could not get KeySet: %v", err)

	k := ks.Lookup(parentKey)
	Checkf(t, err, "could not get key %s: %v", keyName, err)

	if k == nil {
		ks.AppendKey(parentKey)
		Checkf(t, err, "could not append key: %v", err)
		k = parentKey
	}

	for _, m := range meta {
		err = k.SetMeta(m.Key, *m.Value)
		Checkf(t, err, "could not set meta %s = %s: %v", m.Key, *m.Value, err)
	}

	_, err = kdb.Set(ks, errKey)
	Checkf(t, err, "could not save key %s: %v", keyName, err)
}

func testRequest(t *testing.T, verb, path string, body interface{}) *httptest.ResponseRecorder {
	t.Helper()

	var jsonBody io.Reader

	if body != nil {
		marshalled, err := json.Marshal(body)

		Checkf(t, err, "unmarshal %s request: %v", verb, err)

		jsonBody = bytes.NewReader(marshalled)
	}

	r := setupRouter(&server{pool: initPool(10)})

	w := httptest.NewRecorder()

	req, err := http.NewRequest(verb, path, jsonBody)

	Checkf(t, err, "could not create %s request: %v", verb, err)

	r.ServeHTTP(w, req)

	return w
}

func getKey(t *testing.T, keyName string) elektra.Key {
	t.Helper()

	parentKey, err := elektra.NewKey(keyName)
	Checkf(t, err, "could not create key %s: %v", keyName, err)

	kdb := elektra.New()
	err = kdb.Open()
	Checkf(t, err, "could not open kdb: %v", err)

	ks := elektra.NewKeySet()
	_, err = kdb.Get(ks, parentKey)
	Checkf(t, err, "could not get KeySet: %v", err)

	return ks.Lookup(parentKey)
}

func containsMeta(t *testing.T, keyName string, expectedMeta []keyValueBody) {
	key := getKey(t, keyName)
	removeKey(t, keyName)

	for _, actualMeta := range key.MetaSlice() {
		metaName := actualMeta.Name()
		metaValue := actualMeta.String()

		found := false
		for _, expectedMeta := range expectedMeta {
			if expectedMeta.Key == metaName && *expectedMeta.Value == metaValue {
				found = true
			}
		}

		Assertf(t, found, "Expected meta name %s with value %s not found", metaName, metaValue)
	}
}
