package main

import (
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

func main() {
	r := mux.NewRouter()
	r.Use(handleMiddleware)

	r.HandleFunc("/version", GetVersionHandler).Methods("GET")

	r.HandleFunc("/kdbFind/{path:.*}", GetFindHandler).Methods("GET")

	r.HandleFunc("/kdbMv/{path:.*}", PostMoveHandler).Methods("POST")

	r.HandleFunc("/kdbMeta/{path:.*}", PostMetaHandler).Methods("POST")
	r.HandleFunc("/kdbMeta/{path:.*}", DeleteMetaHandler).Methods("DELETE")

	r.HandleFunc("/kdb", GetKdbHandler).Methods("GET")
	r.HandleFunc("/kdb/{path:.*}", GetKdbHandler).Methods("GET")
	r.HandleFunc("/kdb/{path:.*}", PutKdbHandler).Methods("PUT")
	r.HandleFunc("/kdb/{path:.*}", DeleteKdbHandler).Methods("DELETE")

	if err := http.ListenAndServe(":33333", r); err != nil {
		log.Print(err)
	}
}
