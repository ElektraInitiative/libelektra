package main

import (
	"net/http"
)

type versionResult struct {
	API     int            `json:"api"`
	Elektra elektraVersion `json:"elektra"`
}

// getVersionHandler returns the current version of Elektra via
// JSON (see the `versionResult` struct).
//
// Response Code:
//		200 OK
//
// Example: `curl localhost:33333/version`
func (s *server) getVersionHandler(w http.ResponseWriter, r *http.Request) {
	response := versionResult{
		API:     1,
		Elektra: version,
	}

	writeResponse(w, response)
}
