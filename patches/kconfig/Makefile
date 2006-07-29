CC=gcc
CFLAGS=-Wall -ansi `pkg-config qt-mt --cflags`
LDFLAGS=-Wall -ansi `pkg-config qt-mt --libs` -lelektra -lkdecore
EXEC=kde2elektra

all: $(EXEC)

kde2elektra: kde2elektra.o
	$(CC) -o kde2elektra kde2elektra.o $(LDFLAGS)

kde2elektra.o: kde2elektra.cpp
	$(CC) -o kde2elektra.o -c kde2elektra.cpp $(CFLAGS)

clean:
	rm -f *.o

mrproper: clean
	rm -f kde2elektra
