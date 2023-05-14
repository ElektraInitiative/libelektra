package main

import (
	elektra "github.com/ElektraInitiative/libelektra/go-elektra/kdb"
)

type handle struct {
	kdb    elektra.KDB
	keySet elektra.KeySet
}

type handlePool struct {
	handles  chan *handle
	doRefill chan int
	size     int
}

func initPool(size int) *handlePool {
	pool := &handlePool{
		handles:  make(chan *handle, size),
		doRefill: make(chan int, 1),
		size:     size,
	}

	go pool.refillLoop()

	pool.refill()

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

	defer parentKey.Close()

	ks := elektra.NewKeySet()

	if _, err = kdb.Get(ks, parentKey); err != nil {
		return nil, err
	}

	return &handle{
		kdb:    kdb,
		keySet: ks,
	}, nil
}

func (p *handlePool) refill() {
	select {
	case p.doRefill <- 1:
	default:
	}
}

func (p *handlePool) pop() *handle {
	return <-p.handles
}

func (p *handlePool) refillLoop() {
	for range p.doRefill {
		for l := len(p.handles); l < p.size; {
			h, err := newHandle()

			if err != nil {
				panic("could not create new handle: " + err.Error())
			}

			p.handles <- h
		}
	}
}

func (p *handlePool) Get() *handle {
	p.refill()

	return p.pop()
}
