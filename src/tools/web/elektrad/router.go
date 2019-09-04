package main

import (
	"encoding/json"
	"io/ioutil"
	"net/http"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
	"github.com/gorilla/mux"
)

func parseKeyNameFromURL(r *http.Request) string {
	vars := mux.Vars(r)
	keyName := vars["path"]

	if len(keyName) == 0 {
		keyName = "/"
	}

	return keyName
}

func stringBody(r *http.Request) (string, error) {
	value, err := ioutil.ReadAll(r.Body)

	if err != nil {
		return "", err
	}

	return string(value), nil
}

func kdbHandle() elektra.KDB {
	kdb := elektra.New()

	key, _ := elektra.CreateKey("")
	_ = kdb.Open(key)

	return kdb
}

func getKeySet(key elektra.Key) (elektra.KeySet, error) {
	kdb := kdbHandle()

	ks, err := elektra.CreateKeySet()

	if err != nil {
		return nil, err
	}

	_, err = kdb.Get(ks, key)

	if err != nil {
		return nil, err
	}

	return ks, nil
}

func noContent(w http.ResponseWriter) {
	w.WriteHeader(http.StatusNoContent)
}

func badRequest(w http.ResponseWriter) {
	w.WriteHeader(http.StatusBadRequest)
}

func writeError(w http.ResponseWriter, err error) {
	badRequest(w)

	writeResponse(w, map[string]string{
		"error": err.Error(),
	})
}

func writeResponse(w http.ResponseWriter, response interface{}) {
	w.Header().Set("Content-Type", "application/json")

	js, _ := json.Marshal(response)

	w.Write(js)
}
