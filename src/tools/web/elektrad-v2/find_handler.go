package main

import (
	"net/http"
	"regexp"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

func GetFindHandler(w http.ResponseWriter, r *http.Request) {
	query := parseKeyNameFromURL(r)
	regex, err := regexp.Compile(query)

	if err != nil {
		writeError(w, err)
	}

	kdb := kdbHandle()

	ks, err := elektra.CreateKeySet()

	if err != nil {
		writeError(w, err)
		return
	}

	root, err := elektra.CreateKey("/")

	if err != nil {
		writeError(w, err)
		return
	}

	_, err = kdb.Get(ks, root)

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
