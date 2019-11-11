/**
 * @file
 *
 * @brief Tests for the getenv library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <kdbconfig.h>
#include <kdbgetenv.h>

extern pthread_mutex_t elektraGetEnvMutex;

TEST (GetEnv, SimpleFork)
{

	elektraOpen (nullptr, nullptr);
	setenv ("does-exist", "hello", 1);
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
#if VERBOSE
	pid_t f;
	f = fork ();
	std::cerr << "FORK " << f << std::endl;
#else
	fork ();
#endif
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));

#if VERBOSE
	f = fork ();
	std::cerr << "FORK " << f << std::endl;
#else
	fork ();
#endif
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	elektraClose ();
}

#include "main.cpp"
