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
	elektraOpen(0,0);
	ksAppendKey(elektraConfig,
			k = keyNew("user/env/override/does-exist",
				KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext("user/env/override/does-exist");
	EXPECT_EQ(k,f);
}


TEST(Context, ExistWithContext)
{
	Key *k;
	elektraOpen(0,0);
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
	elektraOpen(0,0);
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

namespace ckdb {
void addLayers();
}

TEST(Context, ExistWithContextOverrideCascading)
{
	Key *k;
	elektraOpen(0,0);
	ksAppendKey(elektraConfig,
			keyNew("user/env/layer/layer",
				KEY_VALUE, "layer", KEY_END));
	addLayers();

	ksAppendKey(elektraConfig,
			keyNew("user/env/override/does-exist-too",
				KEY_VALUE, "wrong", KEY_END));
	ksAppendKey(elektraConfig,
			k = keyNew("user/env/override/layer/does-exist-too",
				KEY_VALUE, "correct", KEY_END));
	ksAppendKey(elektraConfig,
			keyNew("spec/env/override/does-exist",
				KEY_META, "context", "/env/override/%layer%/does-exist-too",
				KEY_META, "override/#0", "/env/override/does-exist-too",
				KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext("/env/override/does-exist");
	EXPECT_EQ(k,f);
}

#include "main.cpp"
