package main

import (
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

func main() {
	r := mux.NewRouter()

	r.HandleFunc("/version", GetVersionHandler).Methods("GET")

	r.HandleFunc("/kdb{path:/?.*}", GetKdbHandler).Methods("GET")
	r.HandleFunc("/kdb{path:/?.*}", PutKdbHandler).Methods("PUT")
	r.HandleFunc("/kdb{path:/?.*}", DeleteKdbHandler).Methods("DELETE")

	r.HandleFunc("/kdbFind/{path:.*}", GetKdbFindHandler).Methods("GET")

	if err := http.ListenAndServe(":8080", r); err != nil {
		log.Print(err)
	}
}
