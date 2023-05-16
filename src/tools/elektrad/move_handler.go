package main

import (
	"net/http"

	elektra "go.libelektra.org/src/bindings/go-elektra/kdb"
)

// postMoveHandler moves all keys below the `source` key to the target key.
//
// Arguments:
// 		source	the source key. URL path param.
//		target	the target key. JSON string POST body.
//
// Response Code:
//		204 No Content if succesfull.
//		400 Bad Request if either the source or target keys are invalid.
//
// Example: `curl -X POST -d '"user:/test/world"' localhost:33333/kdbMv/user/test/hello`
func (s *server) postMoveHandler(w http.ResponseWriter, r *http.Request) {
	from := parseKeyNameFromURL(r)
	to, err := stringBody(r)

	if err != nil || from == "" || to == "" {
		badRequest(w)
		return
	}

	fromKey, err := elektra.NewKey(from)

	if err != nil {
		badRequest(w)
		return
	}

	defer fromKey.Close()

	toKey, err := elektra.NewKey(to)

	if err != nil {
		badRequest(w)
		return
	}

	defer toKey.Close()

	root := elektra.CommonKeyName(fromKey, toKey)

	rootKey, err := elektra.NewKey(root)

	if err != nil {
		writeError(w, err) // this should not happen
		return
	}

	defer rootKey.Close()

	handle, conf := getHandle(r)

	_, err = handle.Get(conf, rootKey)

	if err != nil {
		writeError(w, err)
		return
	}

	oldConf := conf.Cut(fromKey)
	defer oldConf.Close()

	if oldConf.Len() < 1 {
		noContent(w)
		return
	}

	newConf := elektra.NewKeySet()
	defer newConf.Close()

	for _, k := range oldConf.ToSlice() {
		newConf.AppendKey(renameKey(k, from, to))
	}

	newConf.Append(conf) // these are unrelated keys

	err = set(handle, newConf, rootKey)

	if err != nil {
		writeError(w, err)
		return
	}

	noContent(w)
}

func renameKey(k elektra.Key, from, to string) elektra.Key {
	otherName := k.Name()
	baseName := otherName[len(from):]

	newKey := k.Duplicate(elektra.KEY_CP_ALL)
	newKey.SetName(to + baseName)

	return newKey
}
