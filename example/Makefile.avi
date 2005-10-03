all:example application binary keyNewExample lookup

example:example.c
	cc -g -L/lib -lelektra -o example example.c

application: application.c
	cc -g -L/lib -lelektra -o application application.c

binary: binary.c
	cc -g -L/lib -lelektra -o binary binary.c

keyNewExample: keyNewExample.c
	cc -g -L/lib -lelektra -o keyNewExample keyNewExample.c

lookup: lookup.c
	cc -g -L/lib -lelektra -o lookup lookup.c


clean:
	rm -f example application binary keyNewExample lookup

