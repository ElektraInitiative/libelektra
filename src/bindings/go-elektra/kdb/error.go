package kdb

import (
	"fmt"

	"errors"
)

type ElektraError struct {
	Err         error
	Description string
	Number      string
	Reason      string
	Ingroup     string
	Module      string
	File        string
	Line        string
}

func (e *ElektraError) Error() string {
	return fmt.Sprintf("%s %s - %s", e.Number, e.Description, e.Reason)
}

func (e *ElektraError) Unwrap() error {
	return e.Err
}

func errFromKey(k *CKey) error {
	description := k.Meta("error/description")
	number := k.Meta("error/number")
	reason := k.Meta("error/reason")
	ingroup := k.Meta("error/ingroup")
	module := k.Meta("error/module")
	file := k.Meta("error/file")
	line := k.Meta("error/line")

	err := errCodeMap[number]

	return &ElektraError{
		Err:         err,
		Description: description,
		Number:      number,
		Reason:      reason,
		Ingroup:     ingroup,
		Module:      module,
		File:        file,
		Line:        line,
	}
}

// error codes taken from libelektra/src/error/specification
var (
	ErrResource            = errors.New("C01100 - Resource")
	ErrOutOfMemory         = errors.New("C01110 - OutOfMemory")
	ErrInstallation        = errors.New("C01200 - Installation")
	ErrInternal            = errors.New("C01310 - Internal")
	ErrInterface           = errors.New("C01320 - Interface")
	ErrPluginMisbehavior   = errors.New("C01330 - PluginMisbehavior")
	ErrConflictingState    = errors.New("C02000 - ConflictingState")
	ErrValidationSyntactic = errors.New("C03100 - ValidationSyntactic")
	ErrValidationSemantic  = errors.New("C03200 - ValidationSemantic")
)

var (
	errCodeMap = map[string]error{
		"C01110": ErrOutOfMemory,
		"C01200": ErrInstallation,
		"C01310": ErrInternal,
		"C01320": ErrInterface,
		"C01330": ErrPluginMisbehavior,
		"C02000": ErrConflictingState,
		"C03100": ErrValidationSyntactic,
		"C03200": ErrValidationSemantic,
	}
)
