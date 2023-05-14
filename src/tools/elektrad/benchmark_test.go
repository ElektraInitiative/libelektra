package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"testing"

	elektra "github.com/ElektraInitiative/libelektra/go-elektra/kdb"
)

type prepareFunc func(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, runs int)

func Benchmark(b *testing.B) {
	world := "world"

	benchmarks := []struct {
		verb      string
		path      string
		body      func(v2 bool, index int) interface{}
		indexKeys bool
	}{
		{
			verb: "GET",
			path: "/version",
		},
		{
			verb: "GET",
			path: "/kdb/user:/tests/go/elektrad/benchmark/get",
		},
		{
			verb: "GET",
			path: "/kdbFind/user:/tests/go/elektrad/benchmark/get/001",
		},
		{
			verb: "POST",
			path: "/kdbMv/user:/tests/go/elektrad/benchmark/post/mv/from",
			body: func(v2 bool, i int) interface{} {
				if v2 {
					return indexedKeyName("/post/mv/to/v2", i)
				}

				return indexedKeyName("/post/mv/to/v1", i)
			},
			indexKeys: true,
		},
		{
			verb: "POST",
			path: "/kdbMeta/user:/tests/go/elektrad/benchmark/post/meta",
			body: func(_ bool, _ int) interface{} {
				return keyValueBody{Key: "hello", Value: &world}
			},
		},
		{
			verb: "PUT",
			path: "/kdb/user:/tests/go/elektrad/benchmark/put",
			body: func(_ bool, _ int) interface{} {
				return "value"
			},
			indexKeys: true,
		},
		{
			verb:      "DELETE",
			path:      "/kdb/user:/tests/go/elektrad/benchmark/delete/kdb",
			indexKeys: true,
		},
		{
			verb: "DELETE",
			path: "/kdbMeta/user:/tests/go/elektrad/benchmark/delete/meta",
			body: func(_ bool, _ int) interface{} {
				return keyValueBody{Key: "hello"}
			},
			indexKeys: true,
		},
	}

	prepareFuncs := []prepareFunc{
		prepareGetKdb,
		preparePostKdbMv,
		preparePostKdbMeta,
		prepareDeleteMeta,
		prepareDeleteKdb,
	}

	cleanup := prepareBenchmark(b, prepareFuncs)
	defer cleanup()

	for _, bt := range benchmarks {
		run := func(b *testing.B, url string, v2 bool) {
			b.StopTimer()
			b.ResetTimer()

			cookie := getCookie(b, url)

			if b.N == 1 {
				return
			}

			for n := 0; n < b.N; n++ {

				path := bt.path

				if bt.indexKeys {
					if v2 {
						path = indexedKey(path+"/v2", n)
					} else {
						path = indexedKey(path+"/v1", n)
					}
				}

				var requestBody interface{}

				if bt.body != nil {
					requestBody = bt.body(v2, n)
				}

				r := benchRequest(b, bt.verb, url, path, requestBody, v2)

				if cookie != nil {
					r.AddCookie(cookie)
				}

				b.StartTimer()
				resp, err := http.DefaultClient.Do(r)
				b.StopTimer()

				Check(b, err, "request failed")

				responseBody, _ := ioutil.ReadAll(resp.Body)

				Assertf(b, resp.StatusCode >= 200 && resp.StatusCode < 300, "unexpected status code for path %s: %d, %s", path, resp.StatusCode, responseBody)

				resp.Body.Close()
			}
		}

		b.Run(fmt.Sprintf("%s%s", bt.verb, bt.path), func(b *testing.B) {
			b.Run("v1", func(b *testing.B) {
				run(b, "http://localhost:33333", false)
			})

			b.Run("v2", func(b *testing.B) {
				run(b, "http://localhost:8080", true)
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

func create(b testing.TB, ks elektra.KeySet, keyName string, count int) {
	for n := 0; n < count; n++ {
		k, err := elektra.NewKey(indexedKeyName(keyName, n))

		Check(b, err, "could not create data key")

		ks.AppendKey(k)
	}
}

var (
	namespace = "user:/tests/go/elektrad/benchmark"
	data      = "/get"
	dataSize  = 100000
)

func indexedKey(keyName string, i int) string {
	return fmt.Sprintf(keyName+"/%06d", i)
}

func indexedKeyName(keyName string, i int) string {
	return indexedKey(namespace+keyName, i)
}

func prepareBenchmark(b *testing.B, prepareFuncs []prepareFunc) func() {
	b.Helper()

	runs := 10 // b.N
	// we have to add a fixed value because the first b.N call is always "1"
	// for initialization reasons.

	handle := getTestHandle(b)

	ks := elektra.NewKeySet()

	parentKey, err := elektra.NewKey(namespace)
	Check(b, err, "could not create parent key")

	get(b, handle, ks, parentKey)
	clear(b, ks, data)

	for _, f := range prepareFuncs {
		f(b, handle, ks, parentKey, runs)
	}

	persist(b, handle, ks, parentKey)

	return func() {
		cleanup(b, handle, ks, parentKey)
	}
}

func prepareGetKdb(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, _ int) {
	create(b, ks, data, dataSize)
}

func preparePostKdbMv(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, runs int) {
	for i := 0; i < runs; i++ {
		create(b, ks, indexedKey("/post/mv/from/v1", i), 10)
	}

	for i := 0; i < runs; i++ {
		create(b, ks, indexedKey("/post/mv/from/v2", i), 10)
	}
}

func prepareDeleteKdb(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, runs int) {
	for i := 0; i < runs; i++ {
		k1, err := elektra.NewKey(indexedKeyName("/delete/kdb/v1", i))
		Check(b, err, "could not create data key")

		k2, err := elektra.NewKey(indexedKeyName("/delete/kdb/v2", i))
		Check(b, err, "could not create data key")

		ks.AppendKey(k1)
		ks.AppendKey(k2)
	}

	for i := 0; i < runs; i++ {
		k, err := elektra.NewKey(indexedKeyName("/delete/kdb", i))
		Check(b, err, "could not create data key")

		ks.AppendKey(k)
	}
}

func preparePostKdbMeta(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, _ int) {
	k, err := elektra.NewKey(namespace + "/post/meta")
	Check(b, err, "could not create data key")

	ks.AppendKey(k)
}

func prepareDeleteMeta(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key, runs int) {
	for i := 0; i < runs; i++ {
		key1, err := elektra.NewKey(indexedKeyName("/delete/meta/v1", i))
		Check(b, err, "could not create data key")

		key2, err := elektra.NewKey(indexedKeyName("/delete/meta/v2", i))
		Check(b, err, "could not create data key")

		ks.AppendKey(key1)
		ks.AppendKey(key2)

		key1.SetMeta("hello", "world")
		key2.SetMeta("hello", "world")
	}
}

func cleanup(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	get(b, handle, ks, parentKey)
	clear(b, ks, namespace)
	persist(b, handle, ks, parentKey)
}
