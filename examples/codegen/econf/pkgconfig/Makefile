KDB ?= kdb

application:../src/application.c genelektra.c
	$(CC) ../src/application.c genelektra.c -std=c99 `pkg-config --cflags --libs elektra-codegen` -I. -o application -Wl,-rpath `pkg-config --variable=libdir elektra-codegen`

genelektra.c:../spec.ini
	$(KDB) gen -F ni=../spec.ini highlevel spec:/sw/example/econf/#0/current genelektra

clean:
	rm -f genelektra.c
	rm -f genelektra.h
	rm -f genelektra.mount.sh
	rm -f application
