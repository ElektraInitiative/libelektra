/**
 * @file
 *
 * @brief Header for filecheck plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_FILECHECK_H
#define ELEKTRA_PLUGIN_FILECHECK_H

#include <elektra/kdbplugin.h>


#define LF_BYTE 0x0A
#define CR_BYTE 0x0D

#define CHAR_BYTES_MAX 4
#define LINE_BYTES 1024

#define INTERNAL_BOM_DELIMITER 0xAA

#define BOM_COUNT 14
#define BOM_SIZE_MAX 5

const uint8_t BOMS[BOM_COUNT][BOM_SIZE_MAX] = {
	{ 0xEF, 0xBB, 0xBF, INTERNAL_BOM_DELIMITER, 0x00 }, { 0xFE, 0xFF, INTERNAL_BOM_DELIMITER, 0x00, 0x00 },
	{ 0xFF, 0xFE, INTERNAL_BOM_DELIMITER, 0x00, 0x00 }, { 0x00, 0x00, 0xFE, 0xFF, INTERNAL_BOM_DELIMITER },
	{ 0xFE, 0xFF, 0x00, 0x00, INTERNAL_BOM_DELIMITER }, { 0x2B, 0x2F, 0x76, 0x38, INTERNAL_BOM_DELIMITER },
	{ 0x2B, 0x2F, 0x76, 0x39, INTERNAL_BOM_DELIMITER }, { 0x2B, 0x2F, 0x76, 0x2B, INTERNAL_BOM_DELIMITER },
	{ 0x2B, 0x2F, 0x76, 0x2F, INTERNAL_BOM_DELIMITER }, { 0xF7, 0x64, 0x4C, INTERNAL_BOM_DELIMITER, 0x00 },
	{ 0xDD, 0x73, 0x66, 0x73, INTERNAL_BOM_DELIMITER }, { 0x0E, 0xFE, 0xFF, INTERNAL_BOM_DELIMITER, 0x00 },
	{ 0xFB, 0xEE, 0x28, INTERNAL_BOM_DELIMITER, 0x00 }, { 0x84, 0x31, 0x95, 0x33, INTERNAL_BOM_DELIMITER }
};

typedef enum
{
	NA,
	CR,
	LF,
	CRLF,
	LFCR,
	NUM_TYPES
} Lineending;

typedef struct
{
	short checkLineEnding;
	Lineending validLE;
	short rejectNullByte;
	short checkEncoding;
	char * encoding;
	short rejectBom;
	short rejectUnprintable;
} checkStruct;


int elektraFilecheckOpen (Plugin * handle, Key * errorKey);
int elektraFilecheckClose (Plugin * handle, Key * errorKey);
int elektraFilecheckGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraFilecheckCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
