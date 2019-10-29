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

type prepareFunc func(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key)

func Benchmark(b *testing.B) {
	s := testServer(b)
	defer s.Close()

	world := "world"

	benchmarks := []struct {
		verb    string
		path    string
		body    interface{}
		prepare prepareFunc
	}{
		{verb: "DELETE", path: "/kdbMeta/user/tests/go/elektrad/benchmark/delete/meta", body: keyValueBody{Key: "hello"}, prepare: prepareDeleteMeta},
		{verb: "DELETE", path: "/kdb/user/tests/go/elektrad/benchmark/delete/kdb", body: "value", prepare: prepareDeleteKdb},
		{verb: "GET", path: "/version"},
		{verb: "GET", path: "/kdb/user/tests/go/elektrad/benchmark/get"},
		{verb: "GET", path: "/kdbFind/user/tests/go/elektrad/benchmark/get/001"},
		{verb: "POST", path: "/kdbMv/user/tests/go/elektrad/benchmark/post/mv/from", body: "user/tests/go/elektrad/benchmark/post/mv/to", prepare: preparePostKdbMv},
		{verb: "POST", path: "/kdbMeta/user/tests/go/elektrad/benchmark/post/meta", body: keyValueBody{Key: "hello", Value: &world}, prepare: preparePostKdbMeta},
		{verb: "PUT", path: "/kdb/user/tests/go/elektrad/benchmark/put", body: "value", prepare: preparePutKdb},
	}

	withHandle := prepareBenchmark(b)
	defer withHandle(cleanup)

	for _, bt := range benchmarks {
		run := func(b *testing.B, url string, v2 bool) {

			r := benchRequest(b, bt.verb, url, bt.path, bt.body, v2)

			b.StopTimer()
			b.ResetTimer()

			for n := 0; n < b.N; n++ {
				if bt.prepare != nil {
					withHandle(bt.prepare)
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

var (
	namespace = "user/tests/go/elektrad/benchmark"
	data      = namespace + "/get"
	dataSize  = 1000
)

func prepareBenchmark(b testing.TB) func(prepareFunc) {
	b.Helper()

	handle := getTestHandle(b)

	ks := elektra.NewKeySet()

	parentKey, err := elektra.NewKey(namespace)
	Check(b, err, "could not create parent key")

	get(b, handle, ks, parentKey)
	clear(b, ks, data)
	create(b, ks, data)
	persist(b, handle, ks, parentKey)

	return func(prepare prepareFunc) {
		prepare(b, handle, ks, parentKey)
	}
}

func preparePostKdbMv(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	t.Helper()

	get(t, handle, ks, parentKey)
	clear(t, ks, namespace+"/post/mv/to")
	create(t, ks, namespace+"/post/mv/from")
	persist(t, handle, ks, parentKey)
}

func preparePutKdb(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	t.Helper()

	get(t, handle, ks, parentKey)

	k := ks.RemoveByName(namespace + "/put")

	if k != nil {
		persist(t, handle, ks, parentKey)
	}
}

func prepareDeleteKdb(t testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	t.Helper()

	get(t, handle, ks, parentKey)

	k, err := elektra.NewKey(namespace + "/delete/kdb")

	Check(t, err, "could not create data key")

	ks.AppendKey(k)

	persist(t, handle, ks, parentKey)
}

func preparePostKdbMeta(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	b.Helper()

	k, err := elektra.NewKey(namespace + "/post/meta")

	Check(b, err, "could not create data key")

	get(b, handle, ks, parentKey)
	ks.AppendKey(k)

	persist(b, handle, ks, parentKey)
}

func prepareDeleteMeta(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	b.Helper()

	get(b, handle, ks, parentKey)

	keyName := namespace + "/delete/meta"

	key := ks.LookupByName(keyName)

	if key == nil {
		var err error

		key, err = elektra.NewKey(namespace + "/delete/meta")
		Check(b, err, "could not create data key")

		ks.AppendKey(key)
	}

	key.SetMeta("hello", "world")

	persist(b, handle, ks, parentKey)
}

func cleanup(b testing.TB, handle elektra.KDB, ks elektra.KeySet, parentKey elektra.Key) {
	b.Helper()

	get(b, handle, ks, parentKey)
	clear(b, ks, namespace)
	persist(b, handle, ks, parentKey)
}
