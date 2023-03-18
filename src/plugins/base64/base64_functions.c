#include "base64_functions.h"
#include <internal/utility/assert.h>
#include <elektra/kdb/errors.h>

static const char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
#define ALPHABET_LENGTH (sizeof (alphabet) - 1)
static const char padding = '=';

/**
 * @brief encodes arbitrary binary data using the Base64 encoding scheme (RFC4648)
 * @param input holds the data to be encoded
 * @param inputLength tells how many bytes the input buffer is holding.
 * @returns an allocated string holding the Base64 encoded input data or NULL if the string can not be allocated. Must be freed by the
	    caller.
 */
char * base64Encode (const kdb_octet_t * input, const size_t inputLength)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[long method]")))
#endif
{
	size_t encodedLength = 0;
	if (inputLength % 3 == 0)
	{
		encodedLength = (inputLength / 3 * 4) + 1;
	}
	else
	{
		encodedLength = ((inputLength + (3 - (inputLength % 3))) / 3 * 4) + 1;
	}
	ELEKTRA_ASSERT (encodedLength > 0, "Base64 output array size smaller or equal to 0.");

	size_t out = 0;
	char * encoded = elektraMalloc (encodedLength);
	if (!encoded) return NULL;

	for (size_t i = 0; i < inputLength; i += 3)
	{
		if (inputLength - i < 3)
		{
			// padding required
			kdb_octet_t padded[3] = { 0 };
			memcpy (padded, input + i, inputLength - i);

			encoded[out++] = alphabet[padded[0] >> 2];
			encoded[out++] = alphabet[((padded[0] << 4) + (padded[1] >> 4)) & 0x3f];

			if (inputLength - i == 2)
			{
				// 2 octets available in input
				encoded[out++] = alphabet[((padded[1] << 2) + (padded[2] >> 6)) & 0x3f];
				encoded[out++] = padding;
			}
			else
			{
				// 1 octet available in input
				encoded[out++] = padding;
				encoded[out++] = padding;
			}
		}
		else
		{
			// no padding required
			encoded[out++] = alphabet[input[i] >> 2];
			encoded[out++] = alphabet[((input[i] << 4) + (input[i + 1] >> 4)) & 0x3f];
			encoded[out++] = alphabet[((input[i + 1] << 2) + (input[i + 2] >> 6)) & 0x3f];
			encoded[out++] = alphabet[input[i + 2] & 0x3f];
		}
	}
	encoded[out++] = '\0';
	return encoded;
}

/**
 * @brief lookup Base64 character in the alphabet and return the index.
 * @param character the character to look up
 * @param errorFlag set to 1 in case of error
 * @returns the index in the Base64 alphabet or 0 if the padding character was detected.
 */
static kdb_octet_t getBase64Index (const char character, int * errorFlag)
{
	if (character == padding) return 0;

	for (kdb_octet_t i = 0; i < ALPHABET_LENGTH; i++)
	{
		if (alphabet[i] == character) return i;
	}
	*errorFlag = 1;
	return 0;
}

/**
 * @brief decodes Base64 encoded data.
 * @param input holds the Base64 encoded data string
 * @param output will be set to an allocated buffer holding the decoded data or NULL if the allocation failed. Must be freed by the caller
	  on success.
 * @param outputLength will be set to the amount of decoded bytes.
 * @retval 1 on success
 * @retval -1 if the provided string has not been encoded with Base64
 * @retval -2 if the output buffer allocation failed
 */
int base64Decode (const char * input, kdb_octet_t ** output, size_t * outputLength)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[high npath complexity]"),
			annotate ("oclint:suppress[long method]"), annotate ("oclint:suppress[high cyclomatic complexity]")))
#endif
{
	const size_t inputLen = strlen (input);
	if (inputLen == 0 || (inputLen == 1 && input[0] == '\0'))
	{
		*output = NULL;
		*outputLength = 0;
		return 1;
	}

	if (inputLen % 4 != 0)
	{
		*output = NULL;
		return -1;
	}

	*outputLength = inputLen / 4 * 3;
	if (input[inputLen - 1] == padding) (*outputLength)--;
	if (input[inputLen - 2] == padding) (*outputLength)--;

	*output = elektraMalloc (*outputLength);
	if (!(*output)) return -2;

	size_t position = 0;
	size_t outputIndex = 0;

	for (position = 0; position < inputLen; position += 4)
	{
		int errorFlag = 0;
		const kdb_octet_t byte0 = getBase64Index (input[position], &errorFlag);
		const kdb_octet_t byte1 = getBase64Index (input[position + 1], &errorFlag);
		const kdb_octet_t byte2 = getBase64Index (input[position + 2], &errorFlag);
		const kdb_octet_t byte3 = getBase64Index (input[position + 3], &errorFlag);

		if (errorFlag)
		{
			// invalid character detected in input string
			elektraFree (*output);
			*output = NULL;
			return -1;
		}

		(*output)[outputIndex++] = (kdb_octet_t) ((byte0 << 2) + (byte1 >> 4));
		if (input[position + 2] != padding)
		{
			(*output)[outputIndex++] = (kdb_octet_t) ((byte1 << 4) + (byte2 >> 2));
		}
		if (input[position + 3] != padding)
		{
			(*output)[outputIndex++] = (kdb_octet_t) ((byte2 << 6) + byte3);
		}
	}
	return 1;
}
