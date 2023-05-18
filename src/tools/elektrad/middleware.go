package main

import (
	"log"
	"net/http"
	"sync"
	"time"

	"github.com/google/uuid"
	"github.com/gorilla/mux"
	elektra "github.com/ElektraInitiative/libelektra/src/bindings/go-elektra/kdb"
)

var (
	sessions sync.Map
	maxAge   = 60 * 60 // 1 hour
)

type session struct {
	handle *handle
	mut    sync.Mutex

	expiry time.Time
}

func handleMiddleware(pool *handlePool) mux.MiddlewareFunc {
	go freeHandles()

	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var s *session

			if cookie, err := r.Cookie("session"); err != nil {
				s = newSession(w, r, pool)
			} else {
				uuid := cookie.Value

				now := time.Now()

				ses, ok := sessions.Load(uuid)

				if s, ok = ses.(*session); !ok || now.After(s.expiry) {
					// the session expired or does not exist, create a new one
					s = newSessionWithUUID(w, r, pool, uuid)
				} else {
					// extend lifetime of session after every request
					s.expiry = sessionExpiry()
				}
			}

			// prevent the handle from being used in parallel
			s.mut.Lock()
			defer s.mut.Unlock()

			next.ServeHTTP(w, r)
		})
	}

}

func freeHandles() {
	for {
		time.Sleep(5 * time.Minute)
		now := time.Now()

		sessions.Range(func(key, value interface{}) bool {
			s := value.(*session)

			if now.After(s.expiry) {
				err := s.handle.kdb.Close()

				if err != nil {
					log.Printf("error closing handle: %v", err)
				}

				s.handle.keySet.Close()

				sessions.Delete(key)
			}

			return true
		})
	}
}

func newSession(w http.ResponseWriter, r *http.Request, pool *handlePool) *session {
	uuid := uuid.New().String()

	return newSessionWithUUID(w, r, pool, uuid)
}

func newSessionWithUUID(w http.ResponseWriter, r *http.Request, pool *handlePool, uuid string) *session {
	cookie := cookieFromUUID(uuid)

	http.SetCookie(w, cookie)
	r.AddCookie(cookie)

	h := pool.Get()

	s := &session{
		handle: h,
		expiry: sessionExpiry(),
	}

	sessions.Store(uuid, s)

	return s
}

func sessionExpiry() time.Time {
	return time.Now().Add(1 * time.Hour)
}

func cookieFromUUID(uuid string) *http.Cookie {
	return &http.Cookie{
		Name:     "session",
		Value:    uuid,
		HttpOnly: true,
		MaxAge:   maxAge,
	}
}

func getHandle(r *http.Request) (elektra.KDB, elektra.KeySet) {
	cookie, err := r.Cookie("session")

	if err != nil {
		panic("handle middleware is not activated (no session cookie)")
	}

	s, ok := sessions.Load(cookie.Value)

	if !ok {
		panic("handle middleware is not activated (no handle)")
	}

	ses := s.(*session)

	return ses.handle.kdb, ses.handle.keySet
}

func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		log.Println(r.Method, r.RequestURI)

		next.ServeHTTP(w, r)
	})
}
