package main

import (
	"net/http"
	"regexp"

	elektra "go.libelektra.org/src/bindings/go-elektra/kdb"
)

// getFindHandler searches for Keys via a Regex expression.
//
// Arguments:
// 		regex	the regex expression to find keys, URL path param.
//
// Response Code:
//		200 OK if the request was successfull.
// 		400 Bad Request if the REGEX is invalid.
//
// Returns: String array with found keys.
//
// Example: `curl localhost:33333/kdbFind/versi*`
func (s *server) getFindHandler(w http.ResponseWriter, r *http.Request) {
	query := parseKeyNameFromURL(r)
	regex, err := regexp.Compile(query)

	if err != nil {
		badRequest(w)
		return
	}

	root, err := elektra.NewKey("/")

	if err != nil {
		writeError(w, err) // this should not happen
		return
	}

	defer root.Close()

	handle, ks := getHandle(r)

	_, err = handle.Get(ks, root)

	if err != nil {
		writeError(w, err)
		return
	}

	results := []string{}

	ks.ForEach(func(key elektra.Key, _ int) {
		name := key.Name()
		if regex.MatchString(name) {
			results = append(results, name)
		}
	})

	writeResponse(w, results)
}
