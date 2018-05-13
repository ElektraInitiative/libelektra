#include <git2.h>
int main (void)
{
	git_libgit2_init ();
	git_index_add_frombuffer (NULL, NULL, NULL, (size_t) NULL);
	git_libgit2_shutdown ();
	return 0;
}
