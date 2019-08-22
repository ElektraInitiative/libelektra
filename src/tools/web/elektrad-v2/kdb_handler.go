package main

import (
	"net/http"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

type LookupResult struct {
	Exists bool     `json:"exists"`
	Name   string   `json:"name"`
	Path   string   `json:"path"`
	Ls     []string `json:"ls"`
	Value  string   `json:"value"`
	Meta   string   `json:"meta"`
}

func GetKdbHandler(w http.ResponseWriter, r *http.Request) {
	kdb := kdbHandle()

	ks, err := elektra.CreateKeySet()

	if err != nil {
		writeError(w, err)
		return
	}

	keyName := parseKeyNameFromURL(r)

	key, _ := elektra.CreateKey(keyName)
	_, err = kdb.Get(ks, key)

	if err != nil {
		writeError(w, err)
		return
	}

	ks = ks.Cut(key)

	key = ks.LookupByName(keyName)

	exists := key != nil
	name := ""
	path := keyName
	ls := ks.KeyNames()
	value := ""

	if exists {
		name = key.BaseName()
		value = key.Value()
	}

	response := LookupResult{
		Exists: exists,
		Name:   name,
		Path:   path,
		Ls:     ls,
		Value:  value,
	}

	writeResponse(w, response)
}

func PutKdbHandler(w http.ResponseWriter, r *http.Request) {
	value, err := StringBody(r)

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
