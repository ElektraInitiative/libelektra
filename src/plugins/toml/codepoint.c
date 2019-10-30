#include "codepoint.h"

#include <kdbassert.h>
#include <stdio.h>

#define TRAIL 0x80
#define MASK(b) (b == 6 ? 0x3F : b == 5 ? 0x1F : b == 4 ? 0x0F : 0x07)
#define GET_BITS(v, s, b) (((v) >> (s)) & MASK (b))
#define LEAD(n) (n == 1 ? 0x00 : n == 2 ? 0xC0 : n == 3 ? 0xE0 : 0xF0)
#define ZERO(n) (n == 1 ? 0x7F : n == 2 ? 0xDF : n == 3 ? 0xEF : 0xF7)
#define ZERO_TRAIL 0xBF

static int utf8LenFromUnicode (unsigned long codepoint);
static unsigned convertCodepoint (const char * codepointStr, int codepointLen);

int utf8FromUnicode (const char * codepointStr, int codepointLen, unsigned char * utf8)
{
	unsigned long codepoint = convertCodepoint (codepointStr, codepointLen);
	int utfLen = utf8LenFromUnicode (codepoint);
	switch (utfLen)
	{
	case 1:
		utf8[0] = (char) codepoint;
		return 1;
	case 2:
		utf8[0] = (unsigned char) ZERO (2) & (LEAD (2) | GET_BITS (codepoint, 6, 5));
		utf8[1] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 0, 6));
		return 2;
	case 3:
		utf8[0] = (unsigned char) ZERO (3) & (LEAD (3) | GET_BITS (codepoint, 6 + 6, 4));
		utf8[1] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 6, 6));
		utf8[2] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 0, 6));
		return 3;
	case 4:
		utf8[0] = (unsigned char) ZERO (4) & (LEAD (4) | GET_BITS (codepoint, 6 + 6 + 6, 3));
		utf8[1] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 6 + 6, 6));
		utf8[2] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 6, 6));
		utf8[3] = (unsigned char) ZERO_TRAIL & (TRAIL | GET_BITS (codepoint, 0, 6));
		return 4;
	default:
		return 0;
	}
}

bool validUtf8FromUnicode (const char * codepointStr, int codepointLen)
{
	return utf8LenFromUnicode (convertCodepoint (codepointStr, codepointLen)) != 0;
}

int utf8LenFromHeadChar (unsigned char head)
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

static unsigned convertCodepoint (const char * codepointStr, int codepointLen)
{
	unsigned long codepoint;
	if (codepointLen == 4)
	{
		sscanf (codepointStr, "%4lX", &codepoint);
	}
	else if (codepointLen == 8)
	{
		sscanf (codepointStr, "%8lX", &codepoint);
	}
	else
	{
		ELEKTRA_ASSERT (0, "Code point len must be 4 or 8, but was %d.", codepointLen);
		return 0;
	}
	return codepoint;
}

static int utf8LenFromUnicode (unsigned long codepoint)
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
