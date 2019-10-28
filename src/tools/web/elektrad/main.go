package main

import (
	"flag"
	"log"
	"net/http"
	"strconv"
	"strings"
)

func main() {
	port := flag.Int("port", 33333, "the port the server listens on")

	flag.Parse()

	loadVersion()

	r := setupRouter()

	if err := http.ListenAndServe(":"+strconv.Itoa(*port), r); err != nil {
		log.Print(err)
	}
}

type elektraVersion struct {
	Version string `json:"version"`
	Major   int    `json:"major"`
	Minor   int    `json:"minor"`
	Micro   int    `json:"micro"`
}

var version elektraVersion

func loadVersion() {
	kdb := newHandle()
	defer kdb.Close()

	versionString, _ := kdb.Version()

	major, minor, micro := parseSemVer(versionString)

	version = elektraVersion{
		Version: versionString,
		Major:   major,
		Minor:   minor,
		Micro:   micro,
	}
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
