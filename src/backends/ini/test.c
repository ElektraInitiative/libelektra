#include "ini.h"

int main ()
{
	FILE * f;
	size_t s;
	int sm;
	char buffer [BUFFER_SIZE];
	
	f = fopen ("value.txt", "w");
	
	s = fread (buffer, sizeof(char), BUFFER_SIZE, stdin);
	sm = convert_strlen (buffer, s);
	fprintf (stderr, "Read %d, will need %d bytes\n",s, sm);
	convert_stream (buffer, s, f);
	
	return 0;
}

