#this Makefile allows you to build some examples
#note that the build is in-tree
#
#You need to have Elektra installed and pkg-config available so that
#it will work.
#
#For compilation, just type:
#
# make
#
#"fatal error: kdb.h" means that you do not have Elektra installed
#properly.

GEN=./gen
ELEKTRA=-lelektra-static
CFLAGS=-O0 -ggdb
FLAGS = `pkg-config --libs --cflags elektra`

.PHONY:all
all:lift cpplift nestedlift lift.html lift.3

# see util.tmpl for motivation
#not used at the moment, do not have dependency to cheetah-compile,
#2 liner should be integrated into kdb gen if approach is chosen
util.py:util.tmpl
	cheetah-compile util.tmpl

lift.html:${GEN} specification.ini template.html
	${GEN} specification.ini template.html > lift.html

lift.3:${GEN} specification.ini template.man
	${GEN} specification.ini template.man > lift.3

lift.h:${GEN} specification.ini template.h c_support.py
	${GEN} specification.ini template.h > lift.h

lift.hpp:${GEN} specification.ini template.hpp
	${GEN} specification.ini template.hpp > lift.hpp

lift_nested.hpp:${GEN} specification.ini template_nested.hpp nested_support.py
	${GEN} specification.ini template_nested.hpp > lift_nested.hpp

genopt.c:${GEN} specification.ini template_genopt.c opt_support.py
	${GEN} specification.ini template_genopt.c > genopt.c

genopt.h:${GEN} specification.ini template_genopt.h opt_support.py
	${GEN} specification.ini template_genopt.h > genopt.h

lift:lift.c lift.h genopt.c genopt.h
	cc ${FLAGS} ${LDFLAGS} ${CFLAGS} -Wall -D _GNU_SOURCE lift.c genopt.c genopt.h lift.h ${ELEKTRA} -o lift

cpplift:lift.cpp lift.hpp
	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall lift.cpp lift.hpp ${ELEKTRA}  -o cpplift

nestedlift:lift_nested.hpp lift_nested.cpp
	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall lift_nested.cpp lift_nested.hpp ${ELEKTRA}  -o nestedlift


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
	rm -f lift_nested.hpp
	rm -f nestedlift
