
/* Subversion stuff

$Id$
$LastChangedBy$

*/


#include <langinfo.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>


int main(int argc, char **argv) {
	printf("Used charset: %s\n", setlocale(LC_ALL,""));
	printf("Used charset: %s\n", nl_langinfo(CODESET));
	printf("Forcing environment\n");
	setenv("LANG","pt_BR.UTF-8",1);
	printf("Used charset: %s\n", setlocale(LC_ALL,""));
	printf("Used charset: %s\n", nl_langinfo(CODESET));
	printf("Again\n");
	printf("Used charset: %s\n", setlocale(LC_ALL,""));
	printf("Used charset: %s\n", nl_langinfo(CODESET));
	return 0;
}
