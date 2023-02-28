#ifndef ELEKTRA_CORE_PRIVATE_H
#define ELEKTRA_CORE_PRIVATE_H

#include "public.h"

struct ElektraNameCow
{
	ElektraName name;

	uint16_t refs;

	bool shouldFree : 1;
	int : 15; // reserved
};

struct ElektraValueCow
{
	ElektraValue value;

	uint16_t refs;

	bool shouldFree : 1; // used for mmap
	int : 15;	     // reserved
};

struct ElektraSet;

struct ElektraEntry
{
	struct ElektraNameCow * name;
	struct ElektraValueCow * value;
	struct ElektraSet * meta;

	uint16_t refs;
	uint16_t nameLock;

	bool shouldFree : 1; // used for mmap
	bool needsSync : 1;
	int : 14; // reserved
};

struct ElektraSetDataCow
{
	struct ElektraEntry ** array;

	size_t size;
	size_t alloc;

	uint16_t refs;

	// Note: OPMPHM parts omitted

	bool shouldFree : 1; // used for mmap
	int : 15;	     // reserved
};

struct ElektraSet
{
	struct ElektraSetDataCow * data;

	uint16_t refs;

	bool shouldFree : 1; // used for mmap
	int : 15;	     // reserved
};

#endif // ELEKTRA_CORE_PRIVATE_H
