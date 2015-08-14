getenv:getenv.c
	gcc -Wl,--wrap=getenv,--wrap=secure_getenv,--wrap=main -o getenv getenv.c `pkg-config --libs --cflags elektra`
