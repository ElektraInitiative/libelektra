#include <langinfo.h>
#include <locale.h>
#include <stdio.h>


int main(int argc, char **argv) {
	setlocale(LC_ALL,"");
	printf("Used charset: %s\n", nl_langinfo(CODESET));
}
