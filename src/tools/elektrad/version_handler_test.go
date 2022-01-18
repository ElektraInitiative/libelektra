package main

import (
	"net/http"
	"testing"
)

func TestGetVersion(t *testing.T) {
	w := testGet(t, "/version")

	code := w.Result().StatusCode
	Assertf(t, code == http.StatusOK, "wrong status code: %v", code)

	var version versionResult

	parseBody(t, w, &version)
	Assert(t, version.API == 1, "version API is not 1")
}
