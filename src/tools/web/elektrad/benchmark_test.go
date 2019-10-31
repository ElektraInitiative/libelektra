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
	"time"

	elektra "go.libelektra.org/kdb"
)

type prepareFunc func(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, n int)

func Benchmark(b *testing.B) {
	s := testServer(b)
	defer s.Close()

	world := "world"

	benchmarks := []struct {
		verb     string
		path     string
		body     interface{}
		prepare  prepareFunc
		indexKey bool
	}{
		{verb: "DELETE", path: "/kdbMeta/user/tests/go/elektrad/benchmark/delete/meta", body: keyValueBody{Key: "hello"}, prepare: prepareDeleteMeta, indexKey: true},
		{verb: "DELETE", path: "/kdb/user/tests/go/elektrad/benchmark/delete/kdb", body: "value", prepare: prepareDeleteKdb, indexKey: true},
		{verb: "GET", path: "/version"},
		{verb: "GET", path: "/kdb/user/tests/go/elektrad/benchmark/get"},
		{verb: "GET", path: "/kdbFind/user/tests/go/elektrad/benchmark/get/001"},
		{verb: "POST", path: "/kdbMv/user/tests/go/elektrad/benchmark/post/mv/from", body: "user/tests/go/elektrad/benchmark/post/mv/to", prepare: preparePostKdbMv, indexKey: true},
		{verb: "POST", path: "/kdbMeta/user/tests/go/elektrad/benchmark/post/meta", body: keyValueBody{Key: "hello", Value: &world}, prepare: preparePostKdbMeta},
		{verb: "PUT", path: "/kdb/user/tests/go/elektrad/benchmark/put", body: "value", indexKey: true},
	}

	withHandle := prepareBenchmark(b)
	defer withHandle(cleanup, 0)

	// wait for server to warmup
	// t := time.Now()
	// time.Sleep(5 * time.Second)
	// b.Logf("resuming tests after waiting for %v", time.Now().Sub(t))

	for _, bt := range benchmarks {
		run := func(b *testing.B, url string, v2 bool) {
			path := bt.path

			b.StopTimer()
			b.ResetTimer()

			cookie := getCookie(b, url)

			if bt.prepare != nil {
				withHandle(bt.prepare, b.N)
			}

			b.Logf("N: %d", b.N)

			for n := 0; n < b.N; n++ {
				if bt.indexKey {
					path = indexedKey(path, n)
				}

				r := benchRequest(b, bt.verb, url, path, bt.body, v2)
				if cookie != nil {
					r.AddCookie(cookie)
				}

				t := time.Now()
				b.StartTimer()
				resp, err := http.DefaultClient.Do(r)
				b.StopTimer()
				b.Logf("time to execute request: %v", time.Now().Sub(t))

				Check(b, err, "request failed")

				body, _ := ioutil.ReadAll(resp.Body)

				Assertf(b, resp.StatusCode >= 200 && resp.StatusCode < 300, "unexpected status code for path %s: %d, %s", path, resp.StatusCode, body)

				resp.Body.Close()
			}
		}

		b.Run(fmt.Sprintf("%s%s", bt.verb, bt.path), func(b *testing.B) {
			b.Run("v1", func(b *testing.B) {
				run(b, "http://localhost:33333", false)
			})

			b.Run("v2", func(b *testing.B) {
				run(b, "http://localhost:8080", true)
				// run(b, s.URL, true)
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

func getCookie(b testing.TB, url string) *http.Cookie {
	r, err := http.NewRequest("GET", url+"/version", nil)
	Check(b, err, "error creating cookie request")

	resp, err := http.DefaultClient.Do(r)
	Check(b, err, "error getting cookie")

	cookies := resp.Cookies()

	if len(cookies) > 0 {
		return cookies[0]
	}

	return nil
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

	router := setupRouter(&server{pool: initPool(1000)})

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

func clear(b testing.TB, ks elektra.KeySet, keyName string) {
	key, err := elektra.NewKey(keyName)
	Check(b, err, "could not create clear key")

	ks.Cut(key)
}

func get(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	b.Helper()

	_, err := handle.Get(ks, parentKey)
	Check(b, err, "could not get test dataset")
}

func persist(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	b.Helper()

	_, err := handle.Set(ks, parentKey)
	Check(b, err, "could not create test dataset")
}

func create(b testing.TB, ks elektra.KeySet, keyName string) {
	doesExist := ks.LookupByName(indexedKeyName(keyName, 1))

	if doesExist != nil {
		return
	}

	for n := 0; n < dataSize; n++ {
		k, err := elektra.NewKey(indexedKeyName(keyName, n))

		Check(b, err, "could not create data key")

		ks.AppendKey(k)
	}
}

var (
	namespace = "user/tests/go/elektrad/benchmark"
	data      = "/get"
	dataSize  = 1000
)

func indexedKey(keyName string, i int) string {
	return fmt.Sprintf(keyName+"/%03d", i)
}

func indexedKeyName(keyName string, i int) string {
	return indexedKey(namespace+keyName, i)
}

func prepareBenchmark(b testing.TB) func(prepareFunc, int) {
	b.Helper()

	handle := getTestHandle(b)

	ks := elektra.NewKeySet()

	parentKey, err := elektra.NewKey(namespace)
	Check(b, err, "could not create parent key")

	get(b, handle, ks, parentKey)
	clear(b, ks, data)
	create(b, ks, data)
	persist(b, handle, ks, parentKey)

	return func(prepare prepareFunc, n int) {
		b.Helper()

		get(b, handle, ks, parentKey)
		prepare(b, handle, ks, parentKey, n)
	}
}

func preparePostKdbMv(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, n int) {
	clear(t, ks, namespace+"/post/mv/to")

	for i := 0; i < n; i++ {
		create(t, ks, indexedKeyName("/post/mv/from", i))
	}

	persist(t, handle, ks, parentKey)
}

func prepareDeleteKdb(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, n int) {
	for i := 0; i < n; i++ {
		k, err := elektra.NewKey(indexedKeyName("/delete/kdb", i))
		Check(t, err, "could not create data key")

		ks.AppendKey(k)
	}

	persist(t, handle, ks, parentKey)
}

func preparePostKdbMeta(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, n int) {
	k, err := elektra.NewKey(namespace + "/post/meta")
	Check(b, err, "could not create data key")

	ks.AppendKey(k)

	persist(b, handle, ks, parentKey)
}

func prepareDeleteMeta(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, n int) {
	for i := 0; i < n; i++ {
		keyName := indexedKeyName("/delete/meta", i)

		key, err := elektra.NewKey(keyName)
		Check(b, err, "could not create data key")
		key.SetMeta("hello", "world")

		ks.AppendKey(key)
	}

	persist(b, handle, ks, parentKey)
}

func cleanup(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, n int) {
	b.Helper()

	get(b, handle, ks, parentKey)
	clear(b, ks, namespace)
	persist(b, handle, ks, parentKey)
}
