application:application.c
	$(CC) application.c `pkg-config --cflags --libs elektra-highlevel` -o application -Wl,-rpath `pkg-config --variable=libdir elektra-highlevel`
