lift:lift.c lift.h lift.html getopt.c
	cc -Wall -lelektra -I/usr/include/elektra lift.c getopt.c lift.h -o lift

lift.html:gen.py specification.ini template.html
	python gen.py specification.ini template.html > lift.html

lift.h:gen.py specification.ini template.h
	python gen.py specification.ini template.h > lift.h

getopt.c:gen.py specification.ini template_getopt.c
	python gen.py specification.ini template_getopt.c > getopt.c

.PHONY:clean
clean:
	rm -f lift.h
	rm -f lift.html
	rm -f getopt.c
	rm -f lift
	rm -f *.pyc
