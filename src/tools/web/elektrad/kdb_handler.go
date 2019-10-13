package main

import (
	"fmt"
	"net/http"
	"strconv"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

// getKdbHandler loads returns various information (see `lookupResult` struct)
// about the Key passed through the URL param.
// The optional `preload` query parameter (int) - default is 0 - determines
// how many levels of Children are recursively loaded.
// Returns 200 OK if the request is successfull.
// Returns 400 Bad Request if the key name or preload (not an integer or
// less than 0 / greater than 9) is invalid.
// Example: `curl localhost:33333/kdb/user/test/hello`
func getKdbHandler(w http.ResponseWriter, r *http.Request) {
	var err error

	preload, err := parsePreload(r)

	if err != nil {
		badRequest(w)
		return
	}

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.NewKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	handle := getHandle(r)
	ks, err := getKeySet(handle, key)

	if err != nil {
		writeError(w, err)
		return
	}

	response, err := lookup(ks, key, preload)

	if err != nil {
		writeError(w, err)
	} else {
		writeResponse(w, response)
	}
}

func lookup(ks elektra.KeySet, key elektra.Key, depth int) (result *lookupResult, err error) {
	ks = ks.Cut(key)

	result = buildLookupResult(key, ks)

	if depth > 0 {
		result.Children, err = lookupChildren(result.Ls, ks, key, depth)
	}

	return
}

type lookupResult struct {
	Exists   bool              `json:"exists"`
	Name     string            `json:"name"`
	Path     string            `json:"path"`
	Ls       []string          `json:"ls"`
	Value    string            `json:"value,omitempty"`
	Meta     map[string]string `json:"meta,omitempty"`
	Children []*lookupResult   `json:"children,omitempty"`
}

func buildLookupResult(key elektra.Key, ks elektra.KeySet) *lookupResult {
	foundKey := ks.Lookup(key)

	var meta map[string]string
	exists := foundKey != nil
	name := key.BaseName()
	value := ""
	path := key.Name()

	if exists {
		value = foundKey.String()
		meta = foundKey.MetaMap()
	}

	ls := ks.KeyNames()

	return &lookupResult{
		Exists: exists,
		Name:   name,
		Path:   path,
		Ls:     ls,
		Value:  value,
		Meta:   meta,
	}
}

func lookupChildren(ls []string, ks elektra.KeySet, key elektra.Key, depth int) ([]*lookupResult, error) {
	var children []*lookupResult

	for _, subKeyName := range ls {
		subKey, err := elektra.NewKey(subKeyName)

		if err != nil {
			return nil, err
		}

		if subKeyName == key.Name() || !key.IsDirectlyBelow(subKey) {
			continue
		}

		childLookup, err := lookup(ks, subKey, depth-1)

		if err != nil {
			return nil, err
		}

		children = append(children, childLookup)
	}

	return children, nil
}

func parsePreload(r *http.Request) (preload int, err error) {
	if preloadQuery, ok := r.URL.Query()["preload"]; ok {
		preload, err = strconv.Atoi(preloadQuery[0])

		if err == nil && preload < 0 || preload > 9 {
			err = fmt.Errorf("invalid preload argument %d", preload)
		}
	}

	return
}

// putKdbHandler creates a new Key, The name of the Key is passed through an
// URL param and the JSON string value via the POST body.
// Returns 201 Created if the operation was succesfull.
// Returns 400 Bad Request if the key name is invalid or the body is not a JSON
// string.
// Example: `curl -X PUT -d '"world"' localhost:33333/kdb/user/test/hello`
func putKdbHandler(w http.ResponseWriter, r *http.Request) {
	value, err := stringBody(r)

	if err != nil {
		badRequest(w)
		return
	}

	kdb := getHandle(r)

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.NewKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	keySet, err := getKeySet(kdb, key)

	if err != nil {
		writeError(w, err)
		return
	}

	err = key.SetString(value)

	if err != nil {
		writeError(w, err)
		return
	}

	keySet.AppendKey(key)

	if err != nil {
		writeError(w, err)
		return
	}

	_, err = kdb.Set(keySet, key)

	if err != nil {
		writeError(w, err)
	} else {
		created(w)
	}
}

// deleteKdbHandler deletes a Key. The name of the Key is passed through the
// URL param.
// Returns 204 No Content if the key was deleted.
// Returns 400 Bad Request if the key name is invalid.
// Example: `curl -X DELETE localhost:33333/kdb/user/test/hello`
func deleteKdbHandler(w http.ResponseWriter, r *http.Request) {
	kdb := getHandle(r)

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.NewKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	keySet, err := getKeySet(kdb, key)

	if err != nil {
		writeError(w, err)
		return
	}

	keySet.Remove(key)

	_, err = kdb.Set(keySet, key)

	if err != nil {
		writeError(w, err)
	} else {
		noContent(w)
	}
}
