#ifndef ELEKTRA_NAMES_PRIVATE_H
#define ELEKTRA_NAMES_PRIVATE_H

#include "public.h"

struct ElektraNameBuffer
{
	char * name;
	size_t size;
	ElektraNamespace ns;
};

#endif // ELEKTRA_NAMES_PRIVATE_H
