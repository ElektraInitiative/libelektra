module github.com/ElektraInitiative/libelektra/elektrad

go 1.13

require (
	github.com/ElektraInitiative/libelektra/go-elektra v0.0.0-00010101000000-000000000000
	github.com/google/uuid v1.3.0
	github.com/gorilla/mux v1.8.0
)

replace github.com/ElektraInitiative/libelektra/go-elektra => ../../bindings/go-elektra
