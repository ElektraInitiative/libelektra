package main

import (
	"net/http"
	"strconv"
	"strings"
)

type versionResult struct {
	API     int            `json:"api"`
	Elektra elektraVersion `json:"elektra"`
}

type elektraVersion struct {
	Version string `json:"version"`
	Major   int    `json:"major"`
	Minor   int    `json:"minor"`
	Micro   int    `json:"micro"`
}

func getVersionHandler(w http.ResponseWriter, r *http.Request) {
	kdb := getHandle(r)

	version, _ := kdb.Version()

	major, minor, micro := parseSemVer(version)

	response := versionResult{
		API: 1,
		Elektra: elektraVersion{
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
