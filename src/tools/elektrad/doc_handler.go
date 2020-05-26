package main

import (
	"net/http"
)

// getDocHandler redirects to the API documentation.
//
// Response Code:
//		301 Moved Permanently Redirect to the documentation.
//
// Example: `curl localhost:33333`
func (s *server) getDocHandler(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "https://elektrad.docs.apiary.io", 301)
}
