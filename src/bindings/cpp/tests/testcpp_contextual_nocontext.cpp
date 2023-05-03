/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/config.h>

#include <kdbvalue.hpp>

#include <gtest/gtest.h>

const uint32_t i_value = 55;
const char * s_value = "55";

TEST (test_contextual_nocontext, integer)
{
	using namespace kdb;
	KeySet ks;
	NoContext c;
	const char * name = "/%language%/%country%/%dialect%/test";
	ASSERT_TRUE (!ks.lookup (name));
	Value<int, ContextPolicyIs<NoContext>> i (ks, c, Key (name, KEY_META, "default", s_value, KEY_END));
	ASSERT_EQ (i, i_value);
	ASSERT_TRUE (ks.lookup (name));
	i = 5;
	ASSERT_EQ (i, 5);
	ASSERT_EQ (i.getSpec ().getName (), name);
	i.syncKeySet ();
	ASSERT_EQ (ks.lookup (name).getString (), "5");
	i = 10;
	ASSERT_EQ (i, 10);
	ASSERT_EQ (ks.lookup (name).getString (), "10");
}
