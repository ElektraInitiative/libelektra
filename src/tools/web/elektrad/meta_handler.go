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

	parentKey, err := elektra.CreateKey(keyName)

	if err != nil {
		writeError(w, err)
		return
	}

	ks, err := getKeySet(kdb, parentKey)

	if err != nil {
		writeError(w, err)
		return
	}

	k, err := ks.Lookup(parentKey)

	if err != nil {
		writeError(w, err)
		return
	}

	if k == nil {
		k = parentKey
		err = ks.AppendKey(parentKey)
	}

	if err != nil {
		writeError(w, err)
		return
	}

	if meta.Value == nil {
		err = k.DeleteMeta(meta.Key)
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

	key, err := elektra.CreateKey(keyName)

	if err != nil {
		writeError(w, err)
		return
	}

	ks, err := getKeySet(kdb, key)

	if err != nil {
		writeError(w, err)
		return
	}

	k, err := ks.Lookup(key)

	if err != nil {
		writeError(w, err)
		return
	}

	if k == nil {
		notFound(w)
		return
	}

	err = k.DeleteMeta(meta.Key)

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
