/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_GOPTS_TESTDATA_H
#define ELEKTRA_GOPTS_TESTDATA_H

#define TEST_EMPTY "empty"
#define TEST_KS_EMPTY ksNew (1, keyNew ("spec:/tests/gopts/key", KEY_META, "default", "5", KEY_END), KS_END)

#define TEST_SINGLEOPT "singleopt"
#define TEST_KS_SINGLEOPT                                                                                                                  \
	ksNew (1, keyNew ("spec:/tests/gopts/key", KEY_META, "opt", "c", KEY_META, "opt/long", "longopt", KEY_END), KS_END)

#define TEST_TWOOPT "twoopt"
#define TEST_KS_TWOOPT                                                                                                                     \
	ksNew (2, keyNew ("spec:/tests/gopts/key", KEY_META, "opt", "c", KEY_META, "opt/long", "longopt", KEY_END),                        \
	       keyNew ("spec:/tests/gopts/key2", KEY_META, "opt", "b", KEY_META, "opt/long", "longopt2", KEY_END), KS_END)

#define TEST_SINGLEENV "singleenv"
#define TEST_KS_SINGLEENV ksNew (1, keyNew ("spec:/tests/gopts/key", KEY_META, "env", "ENV_VAR", KEY_END), KS_END)

#define TEST_TWOENV "twoenv"
#define TEST_KS_TWOENV                                                                                                                     \
	ksNew (2, keyNew ("spec:/tests/gopts/key", KEY_META, "env", "ENV_VAR", KEY_END),                                                   \
	       keyNew ("spec:/tests/gopts/key2", KEY_META, "env", "OTHER_ENV_VAR", KEY_END), KS_END)

#define TEST_MIXED "mixed"
#define TEST_KS_MIXED                                                                                                                      \
	ksNew (2,                                                                                                                          \
	       keyNew ("spec:/tests/gopts/key", KEY_META, "opt", "c", KEY_META, "opt/long", "longopt", KEY_META, "env", "ENV_VAR",         \
		       KEY_END),                                                                                                           \
	       keyNew ("spec:/tests/gopts/key2", KEY_META, "opt", "b", KEY_META, "opt/long", "longopt2", KEY_META, "env", "OTHER_ENV_VAR", \
		       KEY_END),                                                                                                           \
	       KS_END)

#endif // ELEKTRA_GOPTS_TESTDATA_H
