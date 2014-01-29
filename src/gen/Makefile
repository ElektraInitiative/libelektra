.PHONY:all
all:lift cpplift lift.html

lift:lift.c lift.h getopt.c
	cc -Wall -lelektra -I/usr/include/elektra lift.c getopt.c lift.h -o lift

lift.html:gen.py specification.ini template.html
	python gen.py specification.ini template.html > lift.html

lift.h:gen.py specification.ini template.h
	python gen.py specification.ini template.h > lift.h

getopt.c:gen.py specification.ini template_getopt.c
	python gen.py specification.ini template_getopt.c > getopt.c

cpplift:lift.cpp lift.hpp
	c++ -std=c++11 -Wall -lelektra -I/usr/include/elektra lift.cpp lift.hpp -o cpplift

lift.hpp:gen.py specification.ini template.hpp
	python gen.py specification.ini template.hpp > lift.hpp

.PHONY:clean
clean:
	rm -f lift.h lift.hpp
	rm -f lift.html
	rm -f getopt.c
	rm -f lift
	rm -f *.pyc
