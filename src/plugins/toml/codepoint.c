/**
 * @file codepoint.c
 *
 * @brief Functions for converting unicode codepoints to utf8.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "codepoint.h"

#include <internal/utility/assert.h>
#include <stdint.h>
#include <stdio.h>

#include "utf8_dfa.h"

#define TRAIL 0x80
#define MASK(b) (b == 6 ? 0x3F : b == 5 ? 0x1F : b == 4 ? 0x0F : 0x07)
#define GET_BITS(v, s, b) (((v) >> (s)) & MASK (b))
#define LEAD(n) (n == 1 ? 0x00 : n == 2 ? 0xC0 : n == 3 ? 0xE0 : 0xF0)
#define ZERO(n) (n == 1 ? 0x7F : n == 2 ? 0xDF : n == 3 ? 0xEF : 0xF7)
#define ZERO_TRAIL 0xBF

static int utf8LenFromUnicode (uint32_t codepoint);
static uint32_t convertCodepoint (const char * codepointStr, int codepointLen);

bool isValidUtf8 (uint8_t * string, size_t len)
{
	uint32_t codepoint;
	uint32_t state = 0;

	for (size_t i = 0; i < len; ++i)
	{
		if (decode (&state, &codepoint, string[i]) == UTF8_REJECT)
		{
			return false;
		}
	}

	return state == UTF8_ACCEPT;
}

int utf8FromUnicode (const char * codepointStr, int codepointLen, char * utf8)
{
	uint32_t codepoint = convertCodepoint (codepointStr, codepointLen);
	int utfLen = utf8LenFromUnicode (codepoint);
	switch (utfLen)
	{
	case 1:
		utf8[0] = (char) (codepoint & 0x7F);
		return 1;
	case 2:
		utf8[0] = (char) (ZERO (2) & (LEAD (2) | GET_BITS (codepoint, 6, 5)));
		utf8[1] = (char) (ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 0, 6)));
		return 2;
	case 3:
		utf8[0] = (char) (ZERO (3) & (LEAD (3) | GET_BITS (codepoint, 6 + 6, 4)));
		utf8[1] = (char) (ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 6, 6)));
		utf8[2] = (char) (ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 0, 6)));
		return 3;
	case 4:
		utf8[0] = (char) (ZERO (4) & (LEAD (4) | GET_BITS (codepoint, 6 + 6 + 6, 3)));
		utf8[1] = (char) (ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 6 + 6, 6)));
		utf8[2] = (char) (ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 6, 6)));
		utf8[3] = (char) (ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 0, 6)));
		return 4;
	default:
		return 0;
	}
}

int utf8LenFromHeadChar (uint8_t head)
{
	if (head <= 0x7F)
	{
		return 1;
	}
	else if (head >= 0xC0 && head <= 0xDF)
	{
		return 2;
	}
	else if (head >= 0xE0 && head <= 0xEF)
	{
		return 3;
	}
	else if (head >= 0xF0 && head <= 0xF7)
	{
		return 4;
	}
	else
	{
		return 0;
	}
}

static uint32_t convertCodepoint (const char * codepointStr, int codepointLen)
{
	ELEKTRA_ASSERT (codepointLen == 4 || codepointLen == 8, "Code point len must be 4 or 8, but was %d.", codepointLen);

	uint32_t codepoint = 0;
	for (int i = 0; i < codepointLen; i++)
	{
		codepoint <<= 4;

		char c = codepointStr[i];
		if ('0' <= c && c <= '9')
		{
			codepoint |= c - '0';
		}
		else if ('a' <= c && c <= 'f')
		{
			codepoint |= c - 'a' + 0xa;
		}
		else if ('A' <= c && c <= 'F')
		{
			codepoint |= c - 'A' + 0xA;
		}
		else
		{
			ELEKTRA_ASSERT (0, "invalid hex char %c", c);
		}
	}

	return codepoint;
}

static int utf8LenFromUnicode (uint32_t codepoint)
{
	if (codepoint <= 0x7F)
	{
		return 1;
	}
	else if (codepoint >= 0x80 && codepoint <= 0x7FF)
	{
		return 2;
	}
	else if (codepoint >= 0x800 && codepoint <= 0xFFFF)
	{
		return 3;
	}
	else if (codepoint >= 0x10000 && codepoint <= 0x1FFFF)
	{
		return 4;
	}
	else
	{
		return 0;
	}
}
