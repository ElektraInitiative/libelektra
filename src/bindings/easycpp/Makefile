CXX      = g++
#CXXFLAGS = -pipe -Wall -W -ansi -pedantic-errors -g -fno-inline
CXXFLAGS = -pipe -g -fno-inline
INCPATH  = -I.
LINK     = g++
LFLAGS   = 
LIBS     = $(SUBLIBS) -L /lib -lkdb

####### Output directory

OBJECTS_DIR = ./

####### Files

HEADERS = ckdb.h
SOURCES = ckdb.cpp \
		main.cpp
OBJECTS = ckdb.o \
		main.o
FORMS = 
UICDECLS = 
UICIMPLS = 
SRCMOC   = 
OBJMOC = 
DESTDIR  = 
TARGET   = easycpp

first: all
####### Implicit rules

.SUFFIXES: .cpp .o

.cpp.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o $@ $<

####### Build rules

all: $(TARGET)

$(TARGET):  $(OBJECTS)
	$(LINK) $(LFLAGS) -o $(TARGET) $(OBJECTS) $(OBJMOC) $(LIBS) $(OBJCOMP)

clean:
	rm -f $(OBJECTS)


####### Compile

ckdb.o: ckdb.cpp ckdb.h

main.o: main.cpp ckdb.h

