/**
 * @file
 *
 * @brief Declarations for the mmap storage and cache
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef KDBMMAP_H
#define KDBMMAP_H

#include <stddef.h>
#include <stdint.h>

#define ELEKTRA_MAGIC_MMAP_NUMBER (0x0A6172746B656C45)
#define ELEKTRA_MMAP_FORMAT_VERSION (1)

typedef struct _mmapHeader MmapHeader;
typedef struct _mmapMetaData MmapMetaData;
typedef struct _mmapFooter MmapFooter;

/**
 * Flags for mmap
 */
typedef enum {
	MMAP_FLAG_DELETED = 1, /*!<
		 KeySet was deleted, no need to unlink. */
} mmapflag_t;

/**
 * Mmap information header
 *
 * shall contain only fixed-width types
 */
struct _mmapHeader
{
	// clang-format off
	uint64_t mmapMagicNumber;	/**<Magic number for consistency check */
	uint64_t allocSize;		/**<Size of the complete allocation in bytes */
	uint64_t cksumSize;		/**<Size of the critical data for checksum (structs, pointers, sizes)*/

	uint32_t checksum;		/**<Checksum of the data */
	uint8_t formatVersion;		/**<Mmap format version */
	// clang-format on
};

/**
 * Mmap meta-data
 */
struct _mmapMetaData
{
	// clang-format off
	char * destAddr;		/**<Base pointer to allocated destination */

	size_t numKeySets;		/**<Number of KeySets inlcuding meta KS */
	size_t ksAlloc;			/**<Sum of all KeySet->alloc sizes */
	size_t numKeys;			/**<Number of Keys including meta Keys */

	mmapflag_t flags;		/**<Control flags for mmap */
	// clang-format on
};

/**
 * Mmap information footer
 *
 * shall contain only fixed-width types
 */
struct _mmapFooter
{
	// clang-format off
	uint64_t mmapMagicNumber;	/**<Magic number for consistency check */
	// clang-format on
};

#endif
