package main

import (
	"net/http"
	"testing"
)

func TestGetFind(t *testing.T) {
	keyNames := []string{
		"user:/tests/elektrad/kdbfind/get/result1",
		"user:/tests/elektrad/kdbfind/get/result2",
	}

	for _, keyName := range keyNames {
		setupKey(t, keyName)
	}

	w := testGet(t, "/kdbFind/user:/tests/elektrad/kdbfind/get")

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusOK, "wrong status code: %v", code)

	result := []string{}
	parseBody(t, w, &result)
	Assertf(t, len(result) == 2, "result of kdbFind should haven len() 2 but has %d", len(result))
	Assertf(t, keyNames[0] == result[0] && keyNames[1] == result[1], "the result of kdbFind is unexpected")

	for _, keyName := range keyNames {
		removeKey(t, keyName)
	}
}
