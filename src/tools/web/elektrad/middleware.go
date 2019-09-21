package main

import (
	"log"
	"net/http"
	"sync"
	"time"

	"github.com/google/uuid"

	elektra "github.com/ElektraInitiative/go-elektra/kdb"
)

var (
	sessions sync.Map
	maxAge   = 60 * 60 // 1 hour
)

type session struct {
	handle elektra.KDB
	expiry time.Time
	mut    sync.Mutex
}

func handleMiddleware(next http.Handler) http.Handler {
	go freeHandles()

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var s *session

		if cookie, err := r.Cookie("session"); err != nil {
			s = newSession(w, r)
		} else {
			uuid := cookie.Value

			now := time.Now()

			ses, ok := sessions.Load(uuid)

			if s, ok = ses.(*session); !ok && now.After(s.expiry) {
				s = newSessionWithUUID(w, r, uuid)
			}
		}

		// prevent the handle from being used in parallel
		s.mut.Lock()
		defer s.mut.Unlock()

		next.ServeHTTP(w, r)
	})
}

func freeHandles() {
	for {
		time.Sleep(5 * time.Minute)
		now := time.Now()

		sessions.Range(func(key, value interface{}) bool {
			s := value.(*session)

			if now.After(s.expiry) {
				err := s.handle.Close()

				if err != nil {
					log.Printf("error closing handle: %v", err)
				}

				sessions.Delete(key)
			}

			return true
		})
	}
}

func newSession(w http.ResponseWriter, r *http.Request) *session {
	uuid := uuid.New().String()

	return newSessionWithUUID(w, r, uuid)
}

func newSessionWithUUID(w http.ResponseWriter, r *http.Request, uuid string) *session {
	cookie := cookieFromUUID(uuid)

	http.SetCookie(w, cookie)
	r.AddCookie(cookie)

	h := newHandle()

	s := &session{
		handle: h,
		expiry: time.Now().Add(1 * time.Hour),
	}

	sessions.Store(uuid, s)

	return s
}

func cookieFromUUID(uuid string) *http.Cookie {
	return &http.Cookie{
		Name:     "session",
		Value:    uuid,
		HttpOnly: true,
		MaxAge:   maxAge,
	}
}

func newHandle() elektra.KDB {
	kdb := elektra.New()

	err := kdb.Open()

	if err != nil {
		panic(err)
	}

	return kdb
}

func getHandle(r *http.Request) elektra.KDB {
	cookie, err := r.Cookie("session")

	if err != nil {
		panic("handle middleware is not activated (no session cookie)")
	}

	s, ok := sessions.Load(cookie.Value)

	if !ok {
		panic("handle middleware is not activated (no handle)")
	}

	handle := s.(*session).handle

	return handle
}
