package main

import (
	"log"
	"net/http"
)

func main() {
	r := setupRouter()

	if err := http.ListenAndServe(":33333", r); err != nil {
		log.Print(err)
	}
}
