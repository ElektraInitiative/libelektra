package main

import (
	"encoding/json"
	"net/http"

	"github.com/gorilla/mux"

	elektra "go.libelektra.org/kdb"
)

func setupRouter() http.Handler {
	r := mux.NewRouter()
	r.Use(handleMiddleware)

	r.HandleFunc("/version", getVersionHandler).Methods("GET")

	r.HandleFunc("/kdb", getKdbHandler).Methods("GET")
	r.HandleFunc("/kdb/{path:.*}", getKdbHandler).Methods("GET")
	r.HandleFunc("/kdb/{path:.*}", putKdbHandler).Methods("PUT")
	r.HandleFunc("/kdb/{path:.*}", deleteKdbHandler).Methods("DELETE")

	r.HandleFunc("/kdbFind/{path:.*}", getFindHandler).Methods("GET")

	r.HandleFunc("/kdbMv/{path:.*}", postMoveHandler).Methods("POST")

	r.HandleFunc("/kdbMeta/{path:.*}", postMetaHandler).Methods("POST")
	r.HandleFunc("/kdbMeta/{path:.*}", deleteMetaHandler).Methods("DELETE")

	return r
}

func parseKeyNameFromURL(r *http.Request) string {
	vars := mux.Vars(r)
	keyName := vars["path"]

	if len(keyName) == 0 {
		return "/"
	}

	return keyName
}

func stringBody(r *http.Request) (string, error) {
	value := ""

	decoder := json.NewDecoder(r.Body)
	if err := decoder.Decode(&value); err != nil {
		return "", err
	}

	return value, nil
}

func getKeySet(handle elektra.KDB, key elektra.Key) (elektra.KeySet, error) {
	ks := elektra.NewKeySet()

	_, err := handle.Get(ks, key)

	if err != nil {
		return nil, err
	}

	return ks, nil
}

func notFound(w http.ResponseWriter) {
	w.WriteHeader(http.StatusNotFound)
}

func noContent(w http.ResponseWriter) {
	w.WriteHeader(http.StatusNoContent)
}

func badRequest(w http.ResponseWriter) {
	w.WriteHeader(http.StatusBadRequest)
}

func created(w http.ResponseWriter) {
	w.WriteHeader(http.StatusCreated)
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
