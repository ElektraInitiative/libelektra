/**
 * @file
 *
 * @brief Source for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static bool varintRead (FILE * file, kdb_unsigned_long_long_t * result)
{
	kdb_octet_t varintBuf[9];
	kdb_octet_t * varint = varintBuf;

	int c = fgetc (file);
	if (c == EOF)
	{
		return -1;
	}

	varintBuf[0] = c;

	unsigned int ctz = ffs (varint[0]);
	unsigned int len, cnt, i;
	kdb_unsigned_long_long_t num;

	if (ctz == 0)
	{
		len = 8;
		cnt = 8;
		i = 0;
	}
	else
	{
		len = ctz;
		cnt = len - 1;
		i = 1;
	}


	if (fread (&varintBuf[1], sizeof (kdb_octet_t), cnt, file) < cnt)
	{
		return false;
	}

	if (ctz == 0)
	{
		varint++;
		num = 0;
	}
	else
	{
		num = varint[0] >> len;
	}


	for (; i < len; ++i)
	{
		unsigned int shift = i * 8u - ctz;
		num |= (kdb_unsigned_long_long_t) varint[i] << shift;
	}

	*result = num;
	return true;
}

static bool varintWrite (FILE * file, kdb_unsigned_long_long_t num)
{
	kdb_octet_t varint[9];

	unsigned int len = 64 - __builtin_clzll (num | 1u);
	len = 1 + (len - 1) / 7;

	if (len > 8)
	{
		len = 9;
		varint[0] = 0;
	}
	else
	{
		num = ((num << 1u) | 1u) << (len - 1);
		varint[0] = num & 0xFFu;
		num >>= 8u;
	}

	for (unsigned int i = 1; i < len; ++i)
	{
		varint[i] = num & 0xFFu;
		num >>= 8u;
	}

	return fwrite (varint, sizeof (kdb_octet_t), len, file) == len;
}
