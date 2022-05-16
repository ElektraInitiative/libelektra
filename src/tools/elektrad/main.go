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
	initHandles := flag.Int("handles", 10, "count of preinitialized handles")

	flag.Parse()

	err := loadVersion()

	if err != nil {
		log.Fatal(err)
	}

	app := &server{pool: initPool(*initHandles)}

	r := setupRouter(app)

	listen := ":" + strconv.Itoa(*port)

	log.Println("starting elektrad on", listen)

	if err := http.ListenAndServe(listen, r); err != nil {
		log.Print(err)
	}
}

type server struct {
	pool *handlePool
}

type elektraVersion struct {
	Version string `json:"version"`
	Major   int    `json:"major"`
	Minor   int    `json:"minor"`
	Micro   int    `json:"micro"`
}

var version elektraVersion

func loadVersion() error {
	kdb, err := newHandle()

	if err != nil {
		return err
	}

	defer kdb.kdb.Close()

	versionString, _ := kdb.kdb.Version()

	major, minor, micro := parseSemVer(versionString)

	version = elektraVersion{
		Version: versionString,
		Major:   major,
		Minor:   minor,
		Micro:   micro,
	}

	return nil
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
