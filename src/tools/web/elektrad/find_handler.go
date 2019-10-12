package main

import (
	"net/http"
	"regexp"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

func getFindHandler(w http.ResponseWriter, r *http.Request) {
	query := parseKeyNameFromURL(r)
	regex, err := regexp.Compile(query)

	if err != nil {
		badRequest(w)
		return
	}

	root, err := elektra.NewKey("/")

	if err != nil {
		writeError(w, err)
		return
	}

	handle := getHandle(r)
	ks, err := getKeySet(handle, root)

	if err != nil {
		writeError(w, err)
		return
	}

	results := []string{}

	for _, key := range ks.KeyNames() {
		if regex.MatchString(key) {
			results = append(results, key)
		}
	}

	writeResponse(w, results)
}
