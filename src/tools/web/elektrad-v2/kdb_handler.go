package main

import (
	"net/http"
	"strconv"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

type LookupResult struct {
	Exists   bool              `json:"exists"`
	Name     string            `json:"name"`
	Path     string            `json:"path"`
	Ls       []string          `json:"ls"`
	Value    string            `json:"value,omitempty"`
	Meta     map[string]string `json:"meta,omitempty"`
	Children []LookupResult    `json:"children,omitempty"`
}

func GetKdbHandler(w http.ResponseWriter, r *http.Request) {
	var err error
	preload := 0

	if preloadQuery, ok := r.URL.Query()["preload"]; ok {
		preload, err = strconv.Atoi(preloadQuery[0])

		if err != nil || preload < 0 || preload > 9 {
			badRequest(w)
			return
		}
	}

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.CreateKey(keyName)

	if err != nil {
		badRequest(w)
		return
	}

	ks, err := getKeySet(key)

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

func lookup(ks elektra.KeySet, key elektra.Key, depth int) (*LookupResult, error) {
	ks = ks.Cut(key)
	foundKey, err := ks.Lookup(key)

	if err != nil {
		return nil, err
	}

	var meta map[string]string
	exists := foundKey != nil
	name := key.BaseName()
	value := ""
	path := key.Name()

	if exists {
		value = foundKey.Value()
		meta = foundKey.MetaMap()
	}

	ls := ks.KeyNames()

	result := &LookupResult{
		Exists: exists,
		Name:   name,
		Path:   path,
		Ls:     ls,
		Value:  value,
		Meta:   meta,
	}

	if depth > 0 {
		for _, subKeyName := range result.Ls {
			subKey, err := elektra.CreateKey(subKeyName)

			if err != nil {
				return nil, err
			}

			if subKeyName == key.Name() || !key.IsDirectBelow(subKey) {
				continue
			}

			childLookup, err := lookup(ks, subKey, depth-1)

			if err != nil {
				return nil, err
			}

			result.Children = append(result.Children, *childLookup)
		}
	}

	return result, nil
}

func PutKdbHandler(w http.ResponseWriter, r *http.Request) {
	value, err := stringBody(r)

	if err != nil {
		writeError(w, err)
		return
	}

	kdb := kdbHandle()

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.CreateKey(keyName, value)

	if err != nil {
		writeError(w, err)
		return
	}

	keySet, err := elektra.CreateKeySet(key)

	if err != nil {
		writeError(w, err)
		return
	}

	_, err = kdb.Set(keySet, key)

	if err != nil {
		writeError(w, err)
	}
}

func DeleteKdbHandler(w http.ResponseWriter, r *http.Request) {
	kdb := kdbHandle()

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.CreateKey(keyName)

	if err != nil {
		writeError(w, err)
		return
	}

	keySet, err := elektra.CreateKeySet()

	if err != nil {
		writeError(w, err)
		return
	}

	_, err = kdb.Get(keySet, key)

	if err != nil {
		writeError(w, err)
	}

	err = keySet.Remove(key)

	if err != nil {
		writeError(w, err)
	}

	_, err = kdb.Set(keySet, key)

	if err != nil {
		writeError(w, err)
	}
}
