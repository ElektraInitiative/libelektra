package main

import (
	"encoding/json"
	"net/http"

	elektra "go.libelektra.org/src/bindings/go-elektra/kdb"
)

type keyValueBody struct {
	Key   string  `json:"key"`
	Value *string `json:"value"`
}

type metaKeySet struct {
	Meta []keyValueBody `json:"meta"`
}

// postMetaHandler sets a Meta value on a key if a value was passed,
// and deletes the existing Meta value if not.
//
// Arguments:
//
//	keyName the name of the key. URL path param.
//	key		the name of the metaKey. Passed through the key field of the JSON body.
//	value	the value of the metaKey. Passed through the `value` field of the JSON body.
//
// Response Code:
//
//	201 No Content if the request is successful.
//	401 Bad Request if no key name was passed - or the key name is invalid.
//
// Example: `curl -X POST -d '{ "key": "hello", "value": "world" }' localhost:33333/kdbMeta/user/test/hello`
func (s *server) postMetaHandler(w http.ResponseWriter, r *http.Request) {
	var meta keyValueBody

	keyName := parseKeyNameFromURL(r)

	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&meta); err != nil {
		writeError(w, err)
		return
	}

	if meta.Key == "" {
		badRequest(w)
		return
	}

	errKey, err := elektra.NewKey(keyName)

	if err != nil {
		internalServerError(w)
		return
	}

	defer errKey.Close()

	parentKey, err := elektra.NewKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	defer parentKey.Close()

	handle, ks := getHandle(r)

	_, err = handle.Get(ks, errKey)

	if err != nil {
		writeError(w, err)
		return
	}

	k := ks.LookupByName(keyName)

	if k == nil {
		k = parentKey
		ks.AppendKey(parentKey)
	}

	metaKeys := []keyValueBody{meta}
	if err = removeOrSetMetaKeys(k, errKey, handle, ks, metaKeySet{metaKeys}); err != nil {
		writeError(w, err)
		return
	}

	noContent(w)
}

// postMetaBulkHandler sets a whole set of metadata. In case there is a metakey with empty value it deletes the existing
// Meta value.
//
// Arguments:
//
//	keyName the name of the key. URL path param.
//	metaSet the set of metakeys for the given keyName
//
// Response Code:
//
//	201 No Content if the request is successful.
//	401 Bad Request if no key name was passed - or the key name is invalid.
//
// Example: `curl -X POST -d '{ meta: [{"key": "hello", "value": "world"}] }' localhost:33333/kdbMetaBulk/user/test/hello`
func (s *server) postMetaBulkHandler(w http.ResponseWriter, r *http.Request) {
	var metaKeySet metaKeySet

	keyName := parseKeyNameFromURL(r)

	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&metaKeySet); err != nil {
		writeError(w, err)
		return
	}

	if metaKeySet.Meta == nil {
		badRequest(w)
		return
	}

	errKey, err := elektra.NewKey(keyName)

	if err != nil {
		internalServerError(w)
		return
	}

	defer errKey.Close()

	parentKey, err := elektra.NewKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	defer parentKey.Close()

	handle, ks := getHandle(r)

	if _, err = handle.Get(ks, errKey); err != nil {
		writeError(w, err)
		return
	}

	k := ks.LookupByName(keyName)

	if k == nil {
		k = parentKey
		ks.AppendKey(parentKey)
	}

	if err = removeOrSetMetaKeys(k, errKey, handle, ks, metaKeySet); err != nil {
		writeError(w, err)
		return
	}

	noContent(w)
}

// removeOrSetMetaKeys removes or sets a set of metakeys for a given k
//
// Arguments:
//
//	k the key to append metakeys too
//	errKey the key to append errors too
//	handle the KDB handle
//	ks the KeySet the key is located in
//	metaKeys the set of metakeys to append to k
//
// Return:
//
//	error in case it case the set metakey operation failed
func removeOrSetMetaKeys(k elektra.Key, errKey elektra.Key, handle elektra.KDB, ks elektra.KeySet, metaKeys metaKeySet) error {
	var err error

	for _, meta := range metaKeys.Meta {
		if meta.Value == nil {
			err = k.RemoveMeta(meta.Key)
		} else {
			err = k.SetMeta(meta.Key, *meta.Value)
		}

		if err != nil {
			return err
		}

		err = set(handle, ks, errKey)

		if err != nil {
			return err
		}
	}

	return err
}

// deleteMetaHandler deletes a Meta key.
//
// Arguments:
//
//	keyName the name of the Key.
//	key		the name of the metaKey. Passed through the key field of the JSON body.
//
// Response Code:
//
//			201 No Content if the request is successful.
//			401 Bad Request if no key name was passed - or the key name is invalid.
//	     404 Not Found if the key was not found.
//
// Example: `curl -X DELETE -d '{ "key": "hello" }' localhost:33333/kdbMeta/user/test/hello`
func (s *server) deleteMetaHandler(w http.ResponseWriter, r *http.Request) {
	var meta keyValueBody

	keyName := parseKeyNameFromURL(r)

	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&meta); err != nil {
		badRequest(w)
		return
	}

	key, err := elektra.NewKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	defer key.Close()

	errKey, err := elektra.NewKey(keyName)

	if err != nil {
		internalServerError(w)
		return
	}

	defer errKey.Close()

	handle, ks := getHandle(r)

	_, err = handle.Get(ks, errKey)

	if err != nil {
		writeError(w, err)
		return
	}

	k := ks.Lookup(key)

	if k == nil {
		notFound(w)
		return
	}

	err = k.RemoveMeta(meta.Key)

	if err != nil {
		writeError(w, err)
		return
	}

	err = set(handle, ks, errKey)

	if err != nil {
		writeError(w, err)
		return
	}

	noContent(w)
}
