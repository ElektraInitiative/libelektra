package main

import (
	"net/http"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

// postMoveHandler moves all keys below one key to another.
// The source key is passed via the URL param and the target key
// via the JSON string body.
// Returns 204 No Content if succesfull.
// Returns 400 Bad Request if either the source or target keys are invalid.
// Example: `curl -X POST -d '"user/test/world"' localhost:33333/kdbMv/user/test/hello`
func postMoveHandler(w http.ResponseWriter, r *http.Request) {
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

	toKey, err := elektra.NewKey(to)

	if err != nil {
		badRequest(w)
		return
	}

	root := elektra.CommonKeyName(fromKey, toKey)

	rootKey, err := elektra.NewKey(root)

	if err != nil {
		writeError(w, err) // this should not happen
		return
	}

	kdb := getHandle(r)

	conf, err := getKeySet(kdb, rootKey)

	if err != nil {
		writeError(w, err)
		return
	}

	oldConf := conf.Cut(fromKey)

	if oldConf.Len() < 1 {
		noContent(w)
		return
	}

	newConf := elektra.NewKeySet()

	for _, k := range oldConf.Slice() {
		newConf.AppendKey(renameKey(k, from, to))
	}

	newConf.Append(conf) // these are unrelated keys

	_, err = kdb.Set(newConf, rootKey)

	if err != nil {
		writeError(w, err)
	} else {
		noContent(w)
	}
}

func renameKey(k elektra.Key, from, to string) elektra.Key {
	otherName := k.Name()
	baseName := otherName[len(from):]

	newKey := k.Duplicate()
	newKey.SetName(to + baseName)

	return newKey
}
