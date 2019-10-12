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
	}

	kdb := getHandle(r)

	parentKey, err := elektra.NewKey(keyName)

	if err != nil {
		writeError(w, err)
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

func deleteMetaHandler(w http.ResponseWriter, r *http.Request) {
	kdb := getHandle(r)

	var meta keyValueBody

	keyName := parseKeyNameFromURL(r)

	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&meta); err != nil {
		writeError(w, err)
		return
	}

	key, err := elektra.NewKey(keyName)

	if err != nil {
		writeError(w, err)
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
