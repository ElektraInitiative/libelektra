/**
 * \file
 *
 * \brief Tests for the getenv library (context part)
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <kdbgetenv.h>
#include <gtest/gtest.h>

namespace ckdb {
Key *elektraLookupWithContext(std::string name);
}

using namespace ckdb;

TEST(Context, Exist)
{
	Key *k;
	ksAppendKey(elektraConfig,
			k = keyNew("user/env/override/does-exist",
				KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext("user/env/override/does-exist");
	EXPECT_EQ(k,f);
}


TEST(Context, ExistWithContext)
{
	Key *k;
	ksAppendKey(elektraConfig,
			k = keyNew("user/env/override/does-exist",
				KEY_VALUE, "hello", KEY_END));
	ksAppendKey(elektraConfig,
			keyNew("spec/env/override/does-exist",
				KEY_META, "context", "user/env/override/does-exist",
				KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext("/env/override/does-exist");
	EXPECT_EQ(k,f);
}


TEST(Context, ExistWithContextCascading)
{
	Key *k;
	ksAppendKey(elektraConfig,
			k = keyNew("user/env/override/does-exist-too",
				KEY_VALUE, "hello", KEY_END));
	ksAppendKey(elektraConfig,
			keyNew("spec/env/override/does-exist",
				KEY_META, "context", "/env/override/does-exist-too",
				KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext("/env/override/does-exist");
	EXPECT_EQ(k,f);
}

