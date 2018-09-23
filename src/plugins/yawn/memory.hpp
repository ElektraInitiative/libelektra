/**
 * @file
 *
 * @brief This file contains declaration for a class used to allocate memory.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef ELEKTRA_PLUGIN_YAWN_MEMORY_HPP
#define ELEKTRA_PLUGIN_YAWN_MEMORY_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "objstack.h"

// -- Class --------------------------------------------------------------------------------------------------------------------------------

/** This class contains a wrapper for a stack of memory objects. */
class Memory
{
	/* All parser allocated memory is contained in this class. */
	os_t parseTreeMemory;

public:
	/**
	 * This method allocates a memory region of the given size.
	 *
	 * @param size This variable specifies the amount of data this method should
	 *             allocate.
	 *
	 * @return A pointer to a memory region of the specified size
	 */
	void * allocate (int size);
};

#endif // ELEKTRA_PLUGIN_YAWN_MEMORY_HPP
