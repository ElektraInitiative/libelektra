package main

import (
	"encoding/json"
	"net/http"

	"github.com/gorilla/mux"
)

func setupRouter(app *server) http.Handler {
	r := mux.NewRouter()

	r.Use(loggingMiddleware)
	r.Use(handleMiddleware(app.pool))

	r.HandleFunc("/", app.getDocHandler).Methods("GET")

	r.HandleFunc("/version", app.getVersionHandler).Methods("GET")

	r.HandleFunc("/kdb", app.getKdbHandler).Methods("GET")
	r.HandleFunc("/kdb/{path:.*}", app.getKdbHandler).Methods("GET")
	r.HandleFunc("/kdb/{path:.*}", app.putKdbHandler).Methods("PUT")
	r.HandleFunc("/kdb/{path:.*}", app.deleteKdbHandler).Methods("DELETE")

	r.HandleFunc("/kdbFind/{path:.*}", app.getFindHandler).Methods("GET")

	r.HandleFunc("/kdbMv/{path:.*}", app.postMoveHandler).Methods("POST")

	// TODO
	// r.HandleFunc("/kdbCp/{path:.*", app.postCopyHandler).Methods("POST")

	r.HandleFunc("/kdbMeta/{path:.*}", app.postMetaHandler).Methods("POST")
	r.HandleFunc("/kdbMetaBulk/{path:.*}", app.postMetaBulkHandler).Methods("POST")
	r.HandleFunc("/kdbMeta/{path:.*}", app.deleteMetaHandler).Methods("DELETE")

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

func internalServerError(w http.ResponseWriter) {
	w.WriteHeader(http.StatusInternalServerError)
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
