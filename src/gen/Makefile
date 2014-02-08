.PHONY:all
all:lift cpplift lift.html lift.3

util.py:util.tmpl
	cheetah-compile util.tmpl

lift:lift.c lift.h genopt.c genopt.h
	cc -Wall -D _GNU_SOURCE -lelektra -I/usr/include/elektra lift.c genopt.c genopt.h lift.h -o lift

lift.html:gen.py specification.ini template.html
	python gen.py specification.ini template.html > lift.html

lift.3:gen.py specification.ini template.man
	python gen.py specification.ini template.man > lift.3

lift.h:gen.py specification.ini template.h c_support.py
	python gen.py specification.ini template.h > lift.h

genopt.c:gen.py specification.ini template_genopt.c opt_support.py
	python gen.py specification.ini template_genopt.c > genopt.c

genopt.h:gen.py specification.ini template_genopt.h opt_support.py
	python gen.py specification.ini template_genopt.h > genopt.h

cpplift:lift.cpp lift.hpp
	c++ -std=c++11 -Wall -lelektra -I/usr/include/elektra lift.cpp lift.hpp -o cpplift

lift.hpp:gen.py specification.ini template.hpp
	python gen.py specification.ini template.hpp > lift.hpp

.PHONY:clean
clean:
	rm -f lift.h
	rm -f lift.hpp
	rm -f lift.html
	rm -f genopt.c
	rm -f genopt.h
	rm -f lift
	rm -f cpplift
	rm -f *.pyc
	rm -f lift.3
