package main

import (
	"sync"

	elektra "go.libelektra.org/kdb"
)

type handle struct {
	kdb    elektra.KDB
	keySet elektra.KeySet
}

type handlePool struct {
	mut     sync.Mutex
	handles []*handle
}

func initPool(initialSize int) *handlePool {
	pool := &handlePool{
		handles: make([]*handle, initialSize+initialSize*1/4),
	}

	var err error

	for i := 0; i < initialSize; i++ {
		pool.handles[i], err = newHandle()

		if err != nil {
			panic("asd")
		}
	}

	return pool
}

func newHandle() (*handle, error) {
	kdb := elektra.New()

	err := kdb.Open()

	if err != nil {
		return nil, err
	}

	parentKey, err := elektra.NewKey("/")

	if err != nil {
		return nil, err
	}

	ks := elektra.NewKeySet()

	if _, err = kdb.Get(ks, parentKey); err != nil {
		return nil, err
	}

	return &handle{
		kdb:    kdb,
		keySet: ks,
	}, nil
}

func (p *handlePool) pop() *handle {
	var h *handle

	p.mut.Lock()
	size := len(p.handles)

	h, p.handles = p.handles[size-1], p.handles[:size-1]
	p.mut.Unlock()

	return h
}

func (p *handlePool) refill() {
	h, err := newHandle()

	if err != nil {
		panic("could not create new handle")
	}

	p.mut.Lock()
	p.handles = append(p.handles, h)
	defer p.mut.Unlock()
}

func (p *handlePool) Get() *handle {
	p.refill()

	return p.pop()
}
