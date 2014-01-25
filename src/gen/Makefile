lift:lift.c lift.h
	cc -Wall -lelektra -I/usr/include/elektra lift.c lift.h -o lift

lift.h:gen.py specification.ini template.h
	python gen.py specification.ini template.h > lift.h

.PHONY:clean
clean:
	rm -f lift.h
	rm -f lift
