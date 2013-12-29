ex.o:gen.py specification.ini template.c
	python gen.py specification.ini template.c > ex.c
	cc -c -Wall -lelektra -I/usr/include/elektra ex.c -o ex.o

.PHONY:clean
clean:
	rm -f ex.c
	rm -f ex.o
