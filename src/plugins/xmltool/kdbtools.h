/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBTOOLS_H
#define KDBTOOLS_H


#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <stdio.h>


/**
 * Options to change the default behavior of streaming.
 *
 * On default the streaming options only output the names of the given
 * keysets. If you want more information, header, metainfo, compressed
 * output, full names, values or comments you will find the appropriate
 * options here.
 *
 * For full influence of value, comment and metadata shown, use these
 * options together with #elektraKeyFlags. All bits of metainformation ORed
 * together are KDB_O_SHOWMETA.
 *
 * For more information about the flags, consult the documentation of
 * the streaming methods.
 *
 * These options can be ORed. That is the |-Operator in C.
 *
 * It uses the values defined in #elektraKeyFlags too, so it starts with 22.
 *
 * @ingroup stream
 * @see kdbGetChildKeys()
 * @see ksToStream()
 * @see keyToStream()
 */
enum
{
	// clang-format off
	KDB_O_SHOWMETA = 0xF0,       /*!< Show all metadata (type, uid, gid, mode) */
	KDB_O_SHOWFLAGS = 1 << 22,   /*!< Show all flags */
	KDB_O_SHOWINDICES = 1 << 23, /*!< Show the indices for the entries */
	KDB_O_CONDENSED = 1 << 24,   /*!< Spare any whitespace and do not group visually together.*/
	KDB_O_NUMBER = 1 << 25,      /*!< Use a number instead of user and group name.*/
	KDB_O_HEADER = 1 << 26,      /*!< Show also the header of the document. */
	KDB_O_FULLNAME = 1 << 27,    /*!< Export @p user keys using full name.*/
	KDB_O_HIER = 1 << 28	     /*!< Export to the new hierarchical XML
					  representation using key basename.
					  See ksToStream(). */
	// clang-format on
};


#define KDB_SCHEMA_PATH_KEY "system:/elektra/xml/schemapath"

#ifndef DYN_LINK

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef int KDBStream;


typedef int (*KSFromXMLfile) (KeySet * ks, const char * filename);
typedef int (*KSFromXML) (KeySet * ks, int fd);
typedef ssize_t (*output) (const KeySet * ks, FILE * stream, KDBStream options);


int ksFromXMLfile (KeySet * ks, const char * filename);
int ksFromXML (KeySet * ks, int fd);

ssize_t ksToStream (const KeySet * ks, FILE * stream, KDBStream options);
int ksOutput (const KeySet * ks, FILE * stream, KDBStream options);
int ksGenerate (const KeySet * ks, FILE * stream, KDBStream options);

ssize_t keyToStream (const Key * key, FILE * stream, KDBStream options);
ssize_t keyToStreamBasename (const Key * key, FILE * stream, const char * parent, const size_t parentSize, KDBStream options);

int keyOutput (const Key * key, FILE * stream, KDBStream options);
int keyGenerate (const Key * key, FILE * stream, KDBStream options);

#ifdef __cplusplus
}
}
#endif

#endif /* DYN_LINK */

#endif /* KDBTOOLS_H */
