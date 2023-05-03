package main

import (
	"fmt"
	"net/http"
	"strconv"

	elektra "go.libelektra.org/kdb"
)

type keyConfigurationSet struct {
	Configurations []keyValueBody `json:"configurations"`
}

// getKdbHandler loads returns various information about a key.
// about the Key.
//
// Arguments:
//
//	keyName		the name of the key to lookup, URL path param.
//	preload 	determines how many levels of Children are
//				loaded. Optional query parameter (int).
//				Value must be 0-9. Default is 0.
//
// Response Code:
//
//	200 OK if the request is successfull
//	400 Bad Request if the key name or preload is invalid.
//
// Returns: JSON marshaled `lookupResult` struct.
//
// Example: `curl localhost:33333/kdb/user/test/hello`
func (s *server) getKdbHandler(w http.ResponseWriter, r *http.Request) {
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

	defer key.Close()

	handle, ks := getHandle(r)

	errKey, err := elektra.NewKey(keyName)

	if err != nil {
		internalServerError(w)
		return
	}

	defer errKey.Close()

	_, err = handle.Get(ks, errKey)

	if err != nil {
		writeError(w, err)
		return
	}

	dup := ks.Duplicate()
	defer dup.Close()

	response, err := lookup(dup, key, preload)

	if err != nil {
		writeError(w, err)
	} else {
		writeResponse(w, response)
	}
}

// putKdbHandler creates a new Key.
//
// Arguments:
//
//	keyName		the name of the (new) Key. URL path param.
//	value		the (optional) value of the Key. JSON string POST body.
//
// Response Code:
//
//	200 OK if the value was set on an existing key.
//	201 Created if a new key was created.
//	400 Bad Request if the key name is invalid or the body is not a JSON
//
// string.
//
// Example: `curl -X PUT -d '"world"' localhost:33333/kdb/user/test/hello`
func (s *server) putKdbHandler(w http.ResponseWriter, r *http.Request) {
	value, err := stringBody(r)

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

	existingKey := ks.Lookup(key)

	if existingKey != nil {
		key = existingKey
	} else {
		ks.AppendKey(key)
	}

	err = key.SetString(value)

	if err != nil {
		writeError(w, err)
		return
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

	if existingKey == nil {
		created(w)
	}
}

// putAllKdbHandler creating new keys or updating the values of existing keys
//
// Arguments:
//
//	parentKey The key in the path.
//	configurations a list of key-value pairs.
//
// Response Code:
//
//	201 Created if a new key was created.
//	400 Bad Request if the key name is invalid or the body is not a JSON
//
// All keys will be created below the parent key.
//
// Example: `curl -X PUT -d '"world"' localhost:33333/kdbAll/user:/tests`
func (s *server) putAllKdbHandler(w http.ResponseWriter, r *http.Request) {
	var configurations keyConfigurationSet

	parentKey := parseKeyNameFromURL(r)

	if err := parseFor(r, &configurations); err != nil {
		writeError(w, err)
		return
	}

	errKey, err := elektra.NewKey(parentKey)

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

	if err := createOrUpdateValues(parentKey, configurations, ks); err != nil {
		writeError(w, err)
		return
	}

	created(w)
}

func createOrUpdateValues(parentKeyName string, configSet keyConfigurationSet, ks elektra.KeySet) error {
	for _, configuration := range configSet.Configurations {

		newKey, err := elektra.NewKey(parentKeyName + "/" + configuration.Key)
		if err != nil {
			return err
		}

		if err := newKey.SetString(*configuration.Value); err != nil {
			return err
		}

		existingKey := ks.Lookup(newKey)
		if existingKey != nil {
			if err := existingKey.SetString(newKey.String()); err != nil {
				return err
			}
		} else {
			ks.AppendKey(newKey)
		}
	}

	return nil
}

// deleteKdbHandler deletes a Key.
//
// Arguments:
//
//	keyName		the name of the key to be deleted. URL path param.
//
// Response Code:
//
//			204 No Content if the key was deleted.
//			400 Bad Request if the key name is invalid.
//	     404 Not Found if they key to delete was not found.
//
// Example: `curl -X DELETE localhost:33333/kdb/user/test/hello`
func (s *server) deleteKdbHandler(w http.ResponseWriter, r *http.Request) {
	keyName := parseKeyNameFromURL(r)

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

	removedKey := ks.Remove(key)

	if removedKey == nil {
		notFound(w)
		return
	}

	err = set(handle, ks, errKey)

	if err != nil {
		writeError(w, err)
		return
	}

	noContent(w)
}

func lookup(ks elektra.KeySet, key elektra.Key, depth int) (result *lookupResult, err error) {
	childKs := ks.Cut(key)
	defer childKs.Close()

	result = buildLookupResult(key, childKs)

	if depth >= 0 {
		result.Children, err = lookupChildren(childKs, key, depth)
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

func lookupChildren(ks elektra.KeySet, key elektra.Key, depth int) ([]*lookupResult, error) {
	var children []*lookupResult

	ks.ForEach(func(subKey elektra.Key, _ int) {
		if !subKey.IsDirectlyBelow(key) {
			return
		}

		childLookup, err := lookup(ks, subKey, depth-1)

		if err != nil {
			return
		}

		children = append(children, childLookup)
	})

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
