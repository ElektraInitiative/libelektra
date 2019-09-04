package main

import (
	"encoding/json"
	"net/http"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

type KeyValueBody struct {
	Key   string  `json:"key"`
	Value *string `json:"value"`
}

func PostMetaHandler(w http.ResponseWriter, r *http.Request) {
	var meta KeyValueBody

	keyName := parseKeyNameFromURL(r)

	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&meta); err != nil {
		writeError(w, err)
		return
	}

	if meta.Key == "" {
		badRequest(w)
	}

	kdb := kdbHandle()

	ks, err := elektra.CreateKeySet()

	if err != nil {
		writeError(w, err)
		return
	}

	parentKey, _ := elektra.CreateKey(keyName)
	_, err = kdb.Get(ks, parentKey)

	k, err := ks.Lookup(parentKey)

	if k == nil {
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

	noContent(w)
}

func DeleteMetaHandler(w http.ResponseWriter, r *http.Request) {
	// kdb := kdbHandle()

	// ks, err := elektra.CreateKeySet()

	// if err != nil {
	// 	writeError(w, err)
	// 	return
	// }

	// keyName := parseKeyNameFromURL(r)

	// key, _ := elektra.CreateKey(keyName)
	// _, err = kdb.Get(ks, key)

	// writeResponse(w, response)
}
