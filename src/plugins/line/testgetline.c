#include <stdio.h>

int main(void){

	char *line;
	size_t len = 0;
	FILE *fp; 
	getline(&line, &len, fp);
	return 0;
}
