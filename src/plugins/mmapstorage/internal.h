/**
 * @file
 *
 * @brief Internal definitions and macros for mmapstorage and its unit tests
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MMAPSTORAGE_INTERNAL_H
#define ELEKTRA_PLUGIN_MMAPSTORAGE_INTERNAL_H

#define SIZEOF_KEY (sizeof (Key))
#define SIZEOF_KEY_PTR (sizeof (Key *))
#define SIZEOF_KEYSET (sizeof (KeySet))
#define SIZEOF_MMAPHEADER (sizeof (MmapHeader))
#define SIZEOF_MMAPMETADATA (sizeof (MmapMetaData))
#define SIZEOF_MMAPFOOTER (sizeof (MmapFooter))

#define OFFSET_MAGIC_KEYSET (SIZEOF_MMAPHEADER)
#define OFFSET_MAGIC_KEY (OFFSET_MAGIC_KEYSET + SIZEOF_KEYSET)
#define OFFSET_MAGIC_MMAPMETADATA (OFFSET_MAGIC_KEY + SIZEOF_KEY)

#define OFFSET_MMAPMETADATA (OFFSET_MAGIC_MMAPMETADATA + SIZEOF_MMAPMETADATA)
#define OFFSET_GLOBAL_KEYSET (OFFSET_MMAPMETADATA + SIZEOF_MMAPMETADATA)
#define OFFSET_KEYSET (OFFSET_GLOBAL_KEYSET + SIZEOF_KEYSET)

/** Minimum size (lower bound) of mapped region (header, metadata, footer) */
#define ELEKTRA_MMAP_MINSIZE (SIZEOF_MMAPHEADER + (SIZEOF_MMAPMETADATA * 2) + (SIZEOF_KEYSET * 2) + SIZEOF_KEY + SIZEOF_MMAPFOOTER)

/** Magic byte order marker, as used by UTF. */
#define ELEKTRA_MMAP_MAGIC_BOM (0xFEFF)

/** Magic number used in mmap format */
#define ELEKTRA_MAGIC_MMAP_NUMBER (0x0A6172746B656C45)

/** Mmap format version */
#define ELEKTRA_MMAP_FORMAT_VERSION (2)

/** Mmap temp file template */
#define ELEKTRA_MMAP_TMP_NAME "/tmp/elektraMmapTmpXXXXXX"

/** Buffer size for file I/O */
#define ELEKTRA_MMAP_BUFSIZE (4096)

#define STDOUT_FILENAME ("/dev/stdout")
#define STDIN_FILENAME ("/dev/stdin")

/** Suppress warnings in cache mode to debug level */
#define ELEKTRA_MMAP_LOG_WARNING(...)                                                                                                      \
	if (test_bit (mode, MODE_GLOBALCACHE))                                                                                             \
	{                                                                                                                                  \
		ELEKTRA_LOG_DEBUG (__VA_ARGS__);                                                                                           \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		ELEKTRA_LOG_WARNING (__VA_ARGS__);                                                                                         \
	}

/** Flags for mmap file format. We intentionally do not use enums,
	such that the flags can be fit into uint8_t. */

/** Defines whether file was written with checksum on or off. */
#define MMAP_FLAG_CHECKSUM (1)

/** Defines whether file was written with config timestamps (global cache mode). */
#define MMAP_FLAG_TIMESTAMPS (1 << 1)

/**
 * Internal MmapAddr structure.
 * Used for functions passing around relevant pointers into the mmap region.
 * The write functions increment the pointers by the number of bytes written,
 * such that the next iteration or function writes to the correct (free) place.
 */
struct _mmapAddr
{
	// clang-format off
	KeySet * const globalKsPtr;	/**<Pointer to the global KeySet struct. */
	KeySet * const ksPtr;		/**<Pointer to the (main) KeySet struct. */

	char * metaKsPtr;		/**<Pointer to the current meta KeySet structs. */
	char * globalKsArrayPtr;	/**<Pointer to the current global KeySet->array. */
	char * ksArrayPtr;		/**<Pointer to the current KeySet->array. */
	char * metaKsArrayPtr;		/**<Pointer to the current meta KeySet->array. */
	char * keyPtr;			/**<Pointer to the current Key struct. */
	char * dataPtr;			/**<Pointer to the data region, where Key->key and Key->data is stored. */

	const uintptr_t mmapAddrInt;	/**<Address of the mapped region as integer. */
	// clang-format on
};

typedef struct _mmapAddr MmapAddr;

/* Header, metadata and footer needed for mmap file format */
typedef struct _mmapHeader MmapHeader;
typedef struct _mmapMetaData MmapMetaData;
typedef struct _mmapFooter MmapFooter;

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
	uint8_t formatFlags;		/**<Mmap format flags (e.g. checksum ON/OFF) */
	uint8_t formatVersion;		/**<Mmap format version */
	// clang-format on
};

/**
 * Mmap meta-data
 */
struct _mmapMetaData
{
	// clang-format off
	size_t numKeySets;	/**<Number of KeySets inlcuding meta KS */
	size_t ksAlloc;		/**<Sum of all KeySet->alloc sizes */
	size_t numKeys;		/**<Number of Keys including meta Keys */
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
