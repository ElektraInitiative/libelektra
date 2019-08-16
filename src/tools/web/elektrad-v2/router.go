package main

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"strconv"
	"strings"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
	"github.com/gorilla/mux"
)

type VersionResult struct {
	Api     int            `json:"api"`
	Elektra ElektraVersion `json:"elektra"`
}

type ElektraVersion struct {
	Version string `json:"version"`
	Major   int    `json:"major"`
	Minor   int    `json:"minor"`
	Micro   int    `json:"micro"`
}

func GetVersionHandler(w http.ResponseWriter, r *http.Request) {
	kdb := kdbHandle()

	version, _ := kdb.Version()

	major, minor, micro := parseSemVer(version)

	response := VersionResult{
		Api: 1,
		Elektra: ElektraVersion{
			Version: version,
			Major:   major,
			Minor:   minor,
			Micro:   micro,
		},
	}

	writeResponse(w, response)
}

func parseSemVer(version string) (major, minor, micro int) {
	parts := strings.SplitN(version, ".", 3)

	if len(parts) != 3 {
		return
	}

	major, _ = strconv.Atoi(parts[0])
	minor, _ = strconv.Atoi(parts[1])
	micro, _ = strconv.Atoi(parts[2])

	return
}

type LookupResult struct {
	Exists bool     `json:"exists"`
	Name   string   `json:"name"`
	Path   string   `json:"path"`
	Ls     []string `json:"ls"`
	Value  string   `json:"value"`
	Meta   string   `json:"meta"`
}

func GetKdbHandler(w http.ResponseWriter, r *http.Request) {
	kdb := kdbHandle()

	ks, err := elektra.CreateKeySet()

	if err != nil {
		writeError(w, err)
		return
	}

	keyName := parseKeyNameFromURL(r)

	key, _ := elektra.CreateKey(keyName)
	_, err = kdb.Get(ks, key)

	if err != nil {
		writeError(w, err)
		return
	}

	ks = ks.Cut(key)

	key = ks.LookupByName(keyName)

	exists := key != nil
	name := ""
	path := keyName
	ls := ks.KeyNames()
	value := ""

	if exists {
		name = key.BaseName()
		value = key.Value()
	}

	response := LookupResult{
		Exists: exists,
		Name:   name,
		Path:   path,
		Ls:     ls,
		Value:  value,
	}

	writeResponse(w, response)
}

func parseKeyNameFromURL(r *http.Request) string {
	vars := mux.Vars(r)
	keyName := vars["path"]

	if len(keyName) == 0 {
		keyName = "/"
	} else if len(keyName) > 1 {
		keyName = keyName[1:]
	}

	return keyName
}

func PutKdbHandler(w http.ResponseWriter, r *http.Request) {
	value, err := StringBody(r)

	if err != nil {
		writeError(w, err)
		return
	}

	kdb := kdbHandle()

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.CreateKey(keyName, value)

	if err != nil {
		writeError(w, err)
		return
	}

	keySet, err := elektra.CreateKeySet(key)

	if err != nil {
		writeError(w, err)
		return
	}

	_, err = kdb.Set(keySet, key)

	if err != nil {
		writeError(w, err)
	}
}

func DeleteKdbHandler(w http.ResponseWriter, r *http.Request) {
	kdb := kdbHandle()

	keyName := parseKeyNameFromURL(r)

	key, err := elektra.CreateKey(keyName)

	if err != nil {
		writeError(w, err)
		return
	}

	keySet, err := elektra.CreateKeySet()

	if err != nil {
		writeError(w, err)
		return
	}

	_, err = kdb.Get(keySet, key)

	if err != nil {
		writeError(w, err)
	}

	err = keySet.Remove(key)

	if err != nil {
		writeError(w, err)
	}

	_, err = kdb.Set(keySet, key)

	if err != nil {
		writeError(w, err)
	}
}

func GetKdbFindHandler(w http.ResponseWriter, r *http.Request) {

}

func StringBody(r *http.Request) (string, error) {
	value, err := ioutil.ReadAll(r.Body)

	if err != nil {
		return "", err
	}

	return string(value), nil
}

func kdbHandle() elektra.KDB {
	kdb := elektra.New()

	key, _ := elektra.CreateKey("")
	_ = kdb.Open(key)

	return kdb
}

func noContent(w http.ResponseWriter) {
	w.WriteHeader(http.StatusNoContent)
}

func writeError(w http.ResponseWriter, err error) {
	w.WriteHeader(http.StatusBadRequest)

	writeResponse(w, map[string]string{
		"error": err.Error(),
	})
}

func writeResponse(w http.ResponseWriter, response interface{}) {
	w.Header().Set("Content-Type", "application/json")

	js, _ := json.Marshal(response)

	w.Write(js)
}
