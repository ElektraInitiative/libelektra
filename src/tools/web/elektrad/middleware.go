package main

import (
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
}

func handleMiddleware(next http.Handler) http.Handler {
	go freeHandles()

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if cookie, err := r.Cookie("session"); err != nil {
			newSession(w, r)
		} else {
			uuid := cookie.Value

			if _, ok := sessions.Load(uuid); !ok {
				newSessionWithUUID(w, r, uuid)
			}
		}

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
				sessions.Delete(key)
			}

			return true
		})
	}
}

func newSession(w http.ResponseWriter, r *http.Request) {
	uuid := uuid.New().String()

	newSessionWithUUID(w, r, uuid)
}

func newSessionWithUUID(w http.ResponseWriter, r *http.Request, uuid string) {
	cookie := cookieFromUUID(uuid)

	http.SetCookie(w, cookie)
	r.AddCookie(cookie)

	h := newHandle()

	sessions.Store(uuid, &session{
		handle: h,
		expiry: time.Now().Add(1 * time.Hour),
	})
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

	key, _ := elektra.CreateKey("")
	err := kdb.Open(key)

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
