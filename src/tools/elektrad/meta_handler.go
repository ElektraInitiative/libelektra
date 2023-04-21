package main

import (
	"encoding/json"
	"net/http"

	elektra "go.libelektra.org/kdb"
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

	if meta.Value == nil {
		err = k.RemoveMeta(meta.Key)
	} else {
		err = k.SetMeta(meta.Key, *meta.Value)
	}

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

	if metaKeySet.MetaSet == nil {
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

	for _, meta := range metaKeySet.Meta {
		if meta.Value == nil {
			err = k.RemoveMeta(meta.Key)
		} else {
			err = k.SetMeta(meta.Key, *meta.Value)
		}

		if err != nil {
        	writeError(w, err)
        	return
        }

        err = set(handle, ks, errKey)

        if err != nil {
        	writeError(w, err)
        	return
        }
	}

	noContent(w)
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
