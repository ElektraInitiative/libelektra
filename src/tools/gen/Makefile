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
all:lift cpplift nestedlift contextlift contextvisit lift.html lift.3

# see util.tmpl for motivation
#not used at the moment, do not have dependency to cheetah-compile,
#2 liner should be integrated into kdb gen if approach is chosen
util.py:util.tmpl
	cheetah-compile util.tmpl

lift.html:${GEN} specification.ini template.html
	${GEN} specification.ini template.html -o lift.html

lift.3:${GEN} specification.ini template.man
	${GEN} specification.ini template.man -o lift.3

lift.h:${GEN} specification.ini template.h c_support.py
	${GEN} specification.ini template.h -o lift.h

lift.hpp:${GEN} specification.ini template.hpp
	${GEN} specification.ini template.hpp -o lift.hpp

lift_nested.hpp:${GEN} specification.ini template_nested.hpp nested_support.py
	${GEN} specification.ini template_nested.hpp -o lift_nested.hpp

lift_context.hpp:${GEN} specification.ini template_context.hpp context_support.py
	${GEN} specification.ini template_context.hpp -o lift_context.hpp

visit_context.hpp:${GEN} visit_specification.ini template_context.hpp context_support.py
	${GEN} visit_specification.ini template_context.hpp -o visit_context.hpp

genopt.c:${GEN} specification.ini template_genopt.c opt_support.py
	${GEN} specification.ini template_genopt.c -o genopt.c

genopt.h:${GEN} specification.ini template_genopt.h opt_support.py
	${GEN} specification.ini template_genopt.h -o genopt.h

lift:lift.c lift.h genopt.c genopt.h
	cc ${FLAGS} ${LDFLAGS} ${CFLAGS} -Wall -D _GNU_SOURCE lift.c genopt.c genopt.h lift.h ${ELEKTRA} -o lift

cpplift:lift.cpp lift.hpp
	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall lift.cpp lift.hpp ${ELEKTRA}  -o cpplift

nestedlift:lift_nested.hpp lift_nested.cpp
	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall lift_nested.cpp lift_nested.hpp ${ELEKTRA}  -o nestedlift

contextlift:lift_context.hpp lift_context.cpp
	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall lift_context.cpp lift_context.hpp ${ELEKTRA}  -o contextlift

contextvisit:visit_context.hpp visit_context.cpp
	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall visit_context.cpp visit_context.hpp ${ELEKTRA}  -o contextvisit

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
	rm -f lift_context.hpp
	rm -f contextlift
	rm -f visit_context.hpp
	rm -f contextvisit
