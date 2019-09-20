package main

import (
	"flag"
	"log"
	"net/http"
	"strconv"
)

func main() {
	port := flag.Int("port", 33333, "the port the server listens on")

	flag.Parse()

	r := setupRouter()

	if err := http.ListenAndServe(":"+strconv.Itoa(*port), r); err != nil {
		log.Print(err)
	}
}
