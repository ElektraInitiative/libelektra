package main

import (
	"errors"

	elektra "goelektra.libelektra.org/kdb"
)

func set(handle elektra.KDB, ks elektra.KeySet, key elektra.Key) error {
	_, err := handle.Set(ks, key)

	for errors.Is(err, elektra.ErrConflictingState) {
		_, err = handle.Get(ks, key)

		if err != nil {
			return err
		}

		_, err = handle.Set(ks, key)
	}

	return err
}
