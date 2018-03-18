#include <git2.h>
void main (void)
{
	git_libgit2_init ();
	git_index_add_frombuffer (NULL, NULL, NULL, (size_t) NULL);
	git_libgit2_shutdown ();
}
