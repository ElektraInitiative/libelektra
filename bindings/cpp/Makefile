CXX      = g++
#CXXFLAGS = -pipe -Wall -W -ansi -pedantic-errors -g -fno-inline
CXXFLAGS = -pipe -g -fno-inline
INCPATH  = -I.
LINK     = g++
LIBS     = `pkg-config --libs --cflags elektra`

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
TARGET   = cpp

first: all
####### Implicit rules

.SUFFIXES: .cpp .o

.cpp.o:
	$(CXX) -c $(CXXFLAGS) ${LIBS} $(INCPATH) -o $@ $<

####### Build rules

all: $(TARGET)

$(TARGET):  $(OBJECTS)
	$(LINK) $(LIBS) -o $(TARGET) $(OBJECTS) $(OBJMOC) $(LIBS) $(OBJCOMP)

clean:
	rm -f $(OBJECTS)


####### Compile

ckdb.o: ckdb.cpp ckdb.h

main.o: main.cpp ckdb.h

