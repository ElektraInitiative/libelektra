/**
 * @file
 *
 * @brief test if librt functionality is provided by the system.
 * Must only be invoked by CMake's try_compile function.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <sys/wait.h>
#include <sys/mman.h>
#include <sys/stat.h>        /* For mode constants */
#include <fcntl.h>           /* For O_* constants */

int main(int argc, char **argv)
{
	char shm_name[] = "shm_name_elektra_test_compile";
	shm_unlink (shm_name);
	int shm_fd = shm_open (shm_name, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
}