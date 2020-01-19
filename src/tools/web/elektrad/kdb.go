package main

import (
	"errors"

	elektra "go.libelektra.org/kdb"
)

func set(handle elektra.KDB, ks elektra.KeySet, key elektra.Key) error {
	_, err := handle.Set(ks, key)

	if !errors.Is(err, elektra.ErrConflictingState) {
		return err
	}

	_, err = handle.Get(ks, key)

	if err != nil {
		return err
	}

	_, err = handle.Set(ks, key)

	return err
}
