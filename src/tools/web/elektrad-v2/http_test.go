package main_test

import (
	"io/ioutil"
	"net/http"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestKdbGet(t *testing.T) {
	compareGet(t, "/kdb/system/elektra/modules/dump")
}

func TestVersion(t *testing.T) {
	compareGet(t, "/version")
}

func TestKdbPut(t *testing.T) {
	comparePut(t, "/kdb/user/hello", "Testvalue")
}

func comparePut(t *testing.T, path string, body string) {
	req, err := http.NewRequest("PUT", "http://localhost:33333"+path, strings.NewReader(body))
	req.Header.Add("Content-Type", "text/plain")

	if err != nil {
		t.Fatalf("new request failed: %v", err)
	}

	theirResponse, err := http.DefaultClient.Do(req)

	if err != nil {
		t.Fatalf("their http request failed: %v", err)
	}

	req, err = http.NewRequest("PUT", "http://localhost:8080"+path, strings.NewReader(body))

	ourResponse, err := http.DefaultClient.Do(req)

	if err != nil {
		t.Fatalf("our http request failed: %v", err)
	}

	if theirResponse.StatusCode != ourResponse.StatusCode {
		t.Errorf(
			"different status codes: theirs: %d, ours: %d",
			theirResponse.StatusCode,
			ourResponse.StatusCode,
		)
	}

	theirBody, _ := ioutil.ReadAll(theirResponse.Body)

	t.Log(string(theirBody))

	ourBody, _ := ioutil.ReadAll(ourResponse.Body)

	diff := cmp.Diff(theirBody, ourBody)

	if diff != "" {
		t.Errorf("different body:\n%s", diff)
	}
}

func compareGet(t *testing.T, path string) {
	theirResponse, err := http.Get("http://localhost:33333" + path)

	if err != nil {
		t.Fatalf("their http request failed: %v", err)
	}

	ourResponse, err := http.Get("http://localhost:8080" + path)

	if err != nil {
		t.Fatalf("our http request failed: %v", err)
	}

	if theirResponse.StatusCode != ourResponse.StatusCode {
		t.Fatalf(
			"different status codes: theirs: %d, ours: %d",
			theirResponse.StatusCode,
			ourResponse.StatusCode,
		)
	}

	theirBody, _ := ioutil.ReadAll(theirResponse.Body)
	ourBody, _ := ioutil.ReadAll(ourResponse.Body)

	diff := cmp.Diff(theirBody, ourBody)

	if diff != "" {
		t.Fatalf("different body:\n%s", diff)
	}

}
