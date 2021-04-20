package main

import (
	"net/http"
	"testing"
)

func TestGetKdb(t *testing.T) {
	keyName := "user:/tests/elektrad/kdb/get"
	keyNameChild := "user:/tests/elektrad/kdb/get/child"

	setupKey(t, keyName, keyNameChild)

	w := testGet(t, "/kdb/"+keyName)

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusOK, "wrong status code: %v", code)

	var response lookupResult

	parseBody(t, w, &response)

	Assert(t, response.Exists, "key not found")
	Assert(t, response.Path == keyName, "key path is wrong")
	CompareStrings(t, []string{keyName, keyNameChild}, response.Ls, "Children are not the same")

	removeKey(t, keyName)
	removeKey(t, keyNameChild)
}

func TestPutKdb(t *testing.T) {
	keyName := "user:/tests/elektrad/kdb/put"
	value := "test me"

	w := testPut(t, "/kdb/"+keyName, value)

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusCreated, "wrong status code: %v", code)

	key := getKey(t, keyName)
	removeKey(t, keyName)
	Assert(t, key != nil, "key was not created")
	retrievedValue := key.String()
	Assertf(t, retrievedValue == value, "wrong key value %s, expected %s", retrievedValue, value)
}

func TestDeleteKdb(t *testing.T) {
	keyName := "user:/tests/elektrad/kdb/delete"

	setupKey(t, keyName)

	w := testDelete(t, "/kdb/"+keyName, nil)

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusNoContent, "wrong status code: %v", code)

	key := getKey(t, keyName)
	removeKey(t, keyName)
	Assert(t, key == nil, "key was not deleted")
}
