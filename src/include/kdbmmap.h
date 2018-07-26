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
	uint32_t checksum;		/**<Checksum of the data */

	uint8_t sizeofKeySet;
	uint8_t sizeofKey;
	uint8_t sizeofMmapMetaData;
	uint8_t sizeofMmapFooter;

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
	size_t dataSize;		/**<Size of the data block in bytes: dynamic properties like key name, value, etc. */

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
