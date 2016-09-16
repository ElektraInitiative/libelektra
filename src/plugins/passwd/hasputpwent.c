#include <pwd.h>
int main (int argc, char const * argv[])
{
	putpwent (0, 0);
	return 0;
}
