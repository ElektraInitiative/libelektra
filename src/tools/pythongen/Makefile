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

GEN=./pythongen
CFLAGS=-O0 -ggdb
CXXFLAGS=-O0 -ggdb
FLAGS = -I. -I../../bindings/cpp/include `pkg-config --cflags elektra`
CC = gcc
CXX = g++

ELEKTRA_BUILD_TYPE=$(shell pkg-config --variable=build_type elektra)
ifeq (${ELEKTRA_BUILD_TYPE},shared)
	LFLAGS=-lelektra-meta `pkg-config --libs elektra` -pthread -Wl,-rpath `pkg-config --variable=libdir elektra`
else
	LFLAGS=`pkg-config --libs elektra` -pthread -Wl,-rpath `pkg-config --variable=libdir elektra`
endif

.PHONY:all
all:lift cpplift nestedlift dynamiccontextlift lift.html lift.3 location

#staticcontextlift contextvisit contexteditor location

util.py:util/util.c
	${GEN} -p tests/lift.ini util/util.c -o util.py

cpp_util.py:util/util.cpp
	${GEN} -p -c cpp_util tests/lift.ini util/util.cpp -o cpp_util.py

lift.html:${GEN} tests/lift.ini template/template.html util.py
	${GEN} tests/lift.ini template/template.html -o lift.html

lift.3:${GEN} tests/lift.ini  template/template.man util.py
	${GEN} tests/lift.ini template/template.man -o lift.3

lift.h:${GEN} tests/lift.ini  template/template.h support/c.py util.py
	${GEN} tests/lift.ini template/template.h -o lift.h

lift.hpp:${GEN} tests/lift.ini template/template.hpp util.py cpp_util.py
	${GEN} tests/lift.ini template/template.hpp -o lift.hpp

lift_nested.hpp:${GEN} tests/lift.ini template/nested.hpp support/nested.py util.py cpp_util.py
	${GEN} tests/lift.ini template/nested.hpp -o lift_nested.hpp

lift_context_dynamic.hpp:${GEN} tests/lift.ini template/context_dynamic.hpp support/context.py util.py cpp_util.py
	${GEN} tests/lift.ini template/context_dynamic.hpp -o lift_context_dynamic.hpp

location.hpp:${GEN} tests/location.ini template/context_dynamic.hpp support/context.py util.py cpp_util.py
	${GEN} tests/location.ini template/context_dynamic.hpp -o location.hpp

#lift_context_static.hpp:${GEN} tests/lift.ini template/context_static.hpp support/context.py util.py cpp_util.py
#	${GEN} tests/lift.ini template/context_static.hpp -o lift_context_static.hpp
#
#visit_context.hpp:${GEN} tests/visit.ini template/context_static.hpp support/context.py util.py cpp_util.py
#	${GEN} tests/visit.ini template/context_static.hpp -o visit_context.hpp
#
#editor_context.hpp:${GEN} tests/editor.ini template/context_static.hpp support/context.py util.py cpp_util.py
#	${GEN} tests/editor.ini template/context_static.hpp -o editor_context.hpp

genopt.c:${GEN} tests/lift.ini template/genopt.c support/genopt.py util.py
	${GEN} tests/lift.ini template/genopt.c -o genopt.c

genopt.h:${GEN} tests/lift.ini template/genopt.h support/genopt.py util.py
	${GEN} tests/lift.ini template/genopt.h -o genopt.h

lift:tests/lift.c lift.h genopt.c genopt.h
	${CC} ${FLAGS} ${LDFLAGS} ${CFLAGS} -Wall -D _GNU_SOURCE tests/lift.c genopt.c genopt.h lift.h ${LFLAGS} -o lift

cpplift:tests/lift.cpp lift.hpp
	${CXX} ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall tests/lift.cpp lift.hpp ${LFLAGS}  -o cpplift

dynamiccontextlift:lift_context_dynamic.hpp tests/lift_context.cpp
	${CXX} ${FLAGS} ${LDFLAGS} ${CXXFLAGS} -DDYNAMIC  -std=c++11 -Wall tests/lift_context.cpp lift_context_dynamic.hpp ${LFLAGS}  -o dynamiccontextlift

location:location.hpp tests/location.cpp
	${CXX} ${FLAGS} ${LDFLAGS} ${CXXFLAGS} -DDYNAMIC  -std=c++11 -Wall tests/location.cpp location.hpp ${LFLAGS}  -o location

nestedlift:lift_nested.hpp tests/lift_nested.cpp
	${CXX} ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall tests/lift_nested.cpp lift_nested.hpp ${LFLAGS}  -o nestedlift

#staticcontextlift:lift_context_static.hpp tests/lift_context.cpp
#	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall tests/lift_context.cpp lift_context_static.hpp ${LFLAGS}  -o staticcontextlift
#
#contextvisit:visit_context.hpp tests/visit_context.cpp
#	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall tests/visit_context.cpp visit_context.hpp ${LFLAGS}  -o contextvisit
#
#contexteditor:editor_context.hpp tests/editor_context.cpp
#	c++ ${FLAGS} ${LDFLAGS} ${CXXFLAGS}  -std=c++11 -Wall tests/editor_context.cpp editor_context.hpp ${LFLAGS}  -o contexteditor

.PHONY:clean
clean:
	rm -f location.hpp
	rm -f location
	rm -f lift.h
	rm -f lift.hpp
	rm -f lift.html
	rm -f genopt.c
	rm -f genopt.h
	rm -f lift
	rm -f core
	rm -f cpplift
	rm -f *.pyc
	rm -fr *.dSYM
	rm -f lift.3
	rm -f lift_nested.hpp
	rm -f nestedlift
	rm -f lift_context_dynamic.hpp
	rm -f lift_context_static.hpp
	rm -f staticcontextlift
	rm -f dynamiccontextlift
	rm -f visit_context.hpp
	rm -f contextvisit
	rm -f contexteditor
	rm -f editor_context.hpp
	rm -f util.py util.py.bak
	rm -f cpp_util.py cpp_util.py.bak
	rm -f support/__init__.pyc
	rm -f support/c.pyc
	rm -f support/context.pyc
	rm -f support/cpp.pyc
	rm -f support/gen.pyc
	rm -f support/genopt.pyc
	rm -f support/kdb.pyc
	rm -f support/nested.pyc
	rm -f support/util.pyc

