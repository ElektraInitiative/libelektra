/**
 * @file
 *
 * @brief This file contains the implementation of a class used to allocate
 *        memory.
 *
 * About 90 percent of this code was copied from YAEP’s test suite.
 *
 * @copyright GPL License
 *            (see https://github.com/vnmakarov/yaep/blob/master/copyright)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "memory.hpp"

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/**
 * This method allocates a memory region of the given size.
 *
 * @param size This variable specifies the amount of data this method should
 *             allocate.
 *
 * @return A pointer to a memory region of the specified size
 */
void * Memory::allocate (int size)
{
	void * result;

	parseTreeMemory.top_expand (size);
	result = parseTreeMemory.top_begin ();
	parseTreeMemory.top_finish ();

	return result;
}

} // namespace yawn
