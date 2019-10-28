package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"

	elektra "go.libelektra.org/kdb"
)

func Benchmark(b *testing.B) {
	s := testServer(b)
	defer s.Close()

	world := "world"

	benchmarks := []struct {
		verb string
		path string
		body interface{}
	}{
		{verb: "GET", path: "/version"},
		{verb: "GET", path: "/kdb/user/tests/go/elektrad/benchmark/dataset"},
		{verb: "GET", path: "/kdbFind/user/tests/go/elektrad/benchmark/dataset/001"},
		{verb: "POST", path: "/kdbMv/user/tests/go/elektrad/benchmark/temp/from", body: "user/tests/go/elektrad/benchmark/temp/to"},
		{verb: "PUT", path: "/kdb/user/tests/go/elektrad/benchmark/temp/to/001", body: "value"},
		{verb: "POST", path: "/kdbMeta/user/tests/go/elektrad/benchmark/temp/from/001", body: keyValueBody{Key: "hello", Value: &world}},
		{verb: "DELETE", path: "/kdb/user/tests/go/elektrad/benchmark/temp/from/001", body: "value"},
	}

	resetKdb := prepareBenchmark(b, 1000)

	for _, bt := range benchmarks {
		run := func(b *testing.B, url string, v2 bool) {

			r := benchRequest(b, bt.verb, url, bt.path, bt.body, v2)

			b.StopTimer()
			b.ResetTimer()

			for n := 0; n < b.N; n++ {
				if bt.verb != "GET" {
					resetKdb()
				}

				b.StartTimer()
				resp, err := http.DefaultClient.Do(r)
				b.StopTimer()

				body, _ := ioutil.ReadAll(resp.Body)

				Assertf(b, resp.StatusCode >= 200 && resp.StatusCode < 300, "unexpected status code: %d, %s", resp.StatusCode, body)

				Check(b, err, "request failed")

				resp.Body.Close()
			}
		}

		b.Run(fmt.Sprintf("%s%s", bt.verb, bt.path), func(b *testing.B) {
			b.Run("v1", func(b *testing.B) {
				run(b, "http://localhost:33333", false)
			})

			b.Run("v2", func(b *testing.B) {
				run(b, s.URL, true)
			})
		})
	}
}

func stringReader(body string) io.Reader {
	return bytes.NewBuffer([]byte(body))
}

func jsonReader(body interface{}) io.Reader {
	b, err := json.Marshal(body)

	if err != nil {
		panic("could not marshal json")
	}

	return bytes.NewBuffer(b)
}

func getTestHandle(t testing.TB) elektra.KDB {
	handle := elektra.New()
	err := handle.Open()

	if err != nil {
		t.Fatal(err)
	}

	return handle
}

func testServer(t testing.TB) *httptest.Server {
	t.Helper()

	router := setupRouter()

	ts := httptest.NewServer(router)

	return ts
}

func benchRequest(b *testing.B, verb, url, path string, body interface{}, v2 bool) *http.Request {
	b.Helper()

	contentType := "application/json"
	var bodyReader io.Reader

	if body != nil {
		switch b := body.(type) {
		case string:
			if v2 {
				bodyReader = jsonReader(body)
			} else {
				// the old elektra doesn't like JSON strings in the body
				contentType = "text/plain"
				bodyReader = stringReader(b)
			}
		default:
			bodyReader = jsonReader(body)
		}
	}

	r, err := http.NewRequest(verb, url+path, bodyReader)

	r.Header.Add("Content-Type", contentType)

	Check(b, err, "error creating request")

	return r
}

func prepareBenchmark(b testing.TB, dataSize int) func() {
	b.Helper()

	namespace := "user/tests/go/elektrad/benchmark"
	data := namespace + "/dataset"
	temp := namespace + "/temp"
	tempFrom := temp + "/from"
	tempTo := temp + "/to"

	handle := getTestHandle(b)

	ks := elektra.NewKeySet()

	parentKey, err := elektra.NewKey(namespace)
	Check(b, err, "could not create parent key")

	clear := func(keyName string) {
		key, err := elektra.NewKey(keyName)
		Check(b, err, "could not create clear key")

		ks.Cut(key)
	}

	get := func() {
		_, err = handle.Get(ks, parentKey)
		Check(b, err, "could not get test dataset")
	}

	persist := func() {
		_, err = handle.Set(ks, parentKey)
		Check(b, err, "could not create test dataset")
	}

	create := func(keyName string) {
		doesExist := ks.LookupByName(keyName + "/001")

		if doesExist != nil {
			return
		}

		for n := 0; n < dataSize; n++ {
			k, err := elektra.NewKey(fmt.Sprintf(keyName+"/%03d", n))

			Check(b, err, "could not create data key")

			ks.AppendKey(k)
		}
	}

	get()
	clear(data)
	create(data)
	create(tempFrom)
	persist()

	return func() {
		get()
		clear(tempTo)
		create(tempFrom)
		persist()
	}
}
