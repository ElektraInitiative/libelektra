/**
 * @file
 *
 * @brief Tests for the getenv library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <internal/kdb/config.h>
#include <kdbgetenv.h>

namespace ckdb
{
extern pthread_mutex_t elektraGetEnvMutex;
}

TEST (GetEnv, SimpleFork)
{
	using namespace ckdb;
	elektraOpen (nullptr, nullptr);
	setenv ("does-exist", "hello", 1);
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
#if DEBUG
	pid_t f;
	f = fork ();
	std::cerr << "FORK " << f << std::endl;
#else
	fork ();
#endif
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));

#if DEBUG
	f = fork ();
	std::cerr << "FORK " << f << std::endl;
#else
	fork ();
#endif
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	elektraClose ();
}

#include "./main.cpp"
