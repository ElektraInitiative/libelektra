package main

import (
	"encoding/json"
	"net/http"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

type keyValueBody struct {
	Key   string  `json:"key"`
	Value *string `json:"value"`
}

// postMetaHandler sets a Meta value on a key if a value was passed,
// and deletes the existing Meta value if not.
//
// Arguments:
//		keyName the name of the key. URL path param.
//		key		the name of the metaKey. Passed through the key field of the JSON body.
//		value	the value of the metaKey. Passed through the `value` field of the JSON body.
//
// Response Code:
//		201 No Content if the request is successfull.
//		401 Bad Request if no key name was passed - or the key name is invalid.
//
// Example: `curl -X POST -d '{ "key": "hello", "value": "world" }' localhost:33333/kdbMeta/user/test/hello`
func postMetaHandler(w http.ResponseWriter, r *http.Request) {
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

	kdb := getHandle(r)

	parentKey, err := elektra.NewKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	ks, err := getKeySet(kdb, parentKey)

	if err != nil {
		writeError(w, err)
		return
	}

	k := ks.Lookup(parentKey)

	if k == nil {
		k = parentKey
		ks.AppendKey(parentKey)
	}

	if meta.Value == nil {
		err = k.RemoveMeta(meta.Key)
	} else {
		err = k.SetMeta(meta.Key, *meta.Value)
	}

	if err != nil {
		writeError(w, err)
		return
	}

	_, err = kdb.Set(ks, parentKey)

	if err != nil {
		writeError(w, err)
		return
	}

	noContent(w)
}

// deleteMetaHandler deletes a Meta key.
//
// Arguments:
//		keyName the name of the Key.
//		key		the name of the metaKey. Passed through the key field of the JSON body.
//
// Response Code:
//		201 No Content if the request is successfull.
//		401 Bad Request if no key name was passed - or the key name is invalid.
//
// Example: `curl -X DELETE -d '{ "key": "hello" }' localhost:33333/kdbMeta/user/test/hello`
func deleteMetaHandler(w http.ResponseWriter, r *http.Request) {
	kdb := getHandle(r)

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

	ks, err := getKeySet(kdb, key)

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

	_, err = kdb.Set(ks, key)

	if err != nil {
		writeError(w, err)
		return
	}

	noContent(w)
}
