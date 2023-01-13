#ifndef ELEKTRA_CORE_PRIVATE_H
#define ELEKTRA_CORE_PRIVATE_H

#include "public.h"

struct ElektraKeynameCow
{
	ElektraKeyname name;

	uint16_t refs;

	bool shouldFree : 1;
	int : 15; // reserved
};

struct ElektraKeyvalueCow
{
	ElektraKeyvalue value;

	uint16_t refs;

	bool shouldFree : 1; // used for mmap
	int : 15;	     // reserved
};

struct ElektraKeyset;

struct ElektraKey
{
	struct ElektraKeynameCow * name;
	struct ElektraKeyvalueCow * value;
	struct ElektraKeyset * meta;

	uint16_t refs;
	uint16_t nameLock;

	bool shouldFree : 1; // used for mmap
	bool needsSync : 1;
	int : 14; // reserved
};

struct ElektraKeysetDataCow
{
	struct ElektraKey ** array;

	size_t size;
	size_t alloc;

	uint16_t refs;

	// Note: OPMPHM parts omitted

	bool shouldFree : 1; // used for mmap
	int : 15;	     // reserved
};

struct ElektraKeyset
{
	struct ElektraKeysetDataCow * data;

	uint16_t refs;

	bool shouldFree : 1; // used for mmap
	int : 15;	     // reserved
};

#endif // ELEKTRA_CORE_PRIVATE_H
