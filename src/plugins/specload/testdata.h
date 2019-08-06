/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_SPECLOAD_TESTDATA_H
#define ELEKTRA_SPECLOAD_TESTDATA_H

#define PARENT_KEY "spec/tests/specload"

#define DEFAULT_SPEC ksNew (50, keyNew (PARENT_KEY "/mykey", KEY_META, "default", "7", KEY_END), KS_END)

unsigned char default_spec_expected[] = { 0x45, 0x4b, 0x44, 0x42, 0x00, 0x00, 0x00, 0x03, 0x0B, 0x6d, 0x79, 0x6b, 0x65, 0x79,
					  0x73, 0x01, 0x6d, 0x0F, 0x64, 0x65, 0x66, 0x61, 0x75, 0x6c, 0x74, 0x03, 0x37, 0x00 };
unsigned int default_spec_expected_size = 28;

#endif // ELEKTRA_SPECLOAD_TESTDATA_H
