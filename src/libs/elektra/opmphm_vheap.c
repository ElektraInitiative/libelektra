#include "kdbprivate.h"

/**
 * Allocates vheap with size specified by parameter minSize
 *
 * To construct a max heap the comparison function VheapComp (a, b), must return
 * 1 on a > b and 0 otherwise. For a min heap 1 on a < b and 0 otherwise.
 *
 * @ingroup vheap
 * @param comp the comparison function of the heap
 * @param minSize the minimum size of the heap
 * @retval a Vheap pointer
 * @retval NULL error
 */
Vheap * elektraVheapInit (VheapComp comp, size_t minSize)
{
	if (minSize < 1 || !comp) return NULL;
	Vheap * newVheap = elektraMalloc (sizeof (Vheap));
	if (!newVheap) return NULL;
	newVheap->comp = comp;
	newVheap->size = minSize;
	newVheap->minSize = minSize;
	newVheap->count = 0;
	newVheap->data = elektraMalloc (newVheap->size * sizeof (void *));
	if (!newVheap->data)
	{
		elektraFree (newVheap);
		return NULL;
	}
	return newVheap;
}

/**
 * Checks if the heap is empty.
 *
 * @ingroup vheap
 * @retval 1 on empty
 * @retval 0 on non empty
 * @retval -1 on error
 */
int elektraVheapIsEmpty (const Vheap * vheap)
{
	if (!vheap) return -1;
	return (vheap->count == 0);
}

/**
 * Deletes the heap, by freeing all the memory.
 * @ingroup vheap
 *
 */
void elektraVheapDel (Vheap * vheap)
{
	if (!vheap) return;
	elektraFree (vheap->data);
	elektraFree (vheap);
}

/**
 * Inserts an element in the Vheap, by finding the right position.
 * Resizes the memory first if needed.
 *
 * @ingroup vheap
 * @param data the element
 * @retval 0 on error
 * @retval 1 otherwise
 */
int elektraVheapInsert (Vheap * vheap, void * data)
{
	if (!vheap) return 0;
	// grow
	if (++vheap->count > vheap->size)
	{
		vheap->size <<= 1;
		if (elektraRealloc ((void **)&vheap->data, vheap->size * sizeof (void *)) == -1)
		{
			--vheap->count;
			vheap->size >>= 1;
			return 0;
		}
	}
	size_t parent, pos;
	for (pos = vheap->count - 1; pos; pos = parent)
	{
		// calculates the position of the parent in the data array
		parent = (pos - 1) >> 1;
		if (vheap->comp (vheap->data[parent], data)) break;
		vheap->data[pos] = vheap->data[parent];
	}
	vheap->data[pos] = data;
	return 1;
}

/**
 * Removes and returns an element from the Vheap, by taking the first
 * element which is ordered by VheapComp, after removal the remaining elements
 * get ordered again. Resizes the heap if needed.
 *
 * @ingroup vheap
 * @retval the element
 * @retval NULL on error
 */
void * elektraVheapRemove (Vheap * vheap)
{
	if (!vheap) return NULL;
	if (elektraVheapIsEmpty (vheap)) return NULL;
	// shrink
	--vheap->count;
	// check size against min size fist
	if (vheap->size > vheap->minSize && vheap->count <= vheap->size >> 2)
	{
		vheap->size >>= 1;
		if (elektraRealloc ((void **)&vheap->data, vheap->size * sizeof (void *)) == -1)
		{
			++vheap->count;
			vheap->size <<= 1;
			return NULL;
		}
	}
	void * ret = vheap->data[0];
	vheap->data[0] = vheap->data[vheap->count];
	size_t elem, child;
	elem = 0;
	child = 1;
	int comp = 1;
	while (comp && child < vheap->count)
	{
		if (child + 1 < vheap->count && vheap->comp (vheap->data[child + 1], vheap->data[child]))
		{
			++child;
		}
		comp = vheap->comp (vheap->data[child], vheap->data[elem]);
		if (comp)
		{
			void * temp = vheap->data[child];
			vheap->data[child] = vheap->data[elem];
			vheap->data[elem] = temp;
			elem = child;
			child = (child << 1) + 1;
		}
	}
	return ret;
}

/**
 * Clears the heap in a fast fashion.
 * Set the count to zero and does not reallocate memory.
 * The next reallocation happens at Remove.
 *
 * @ingroup vheap
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraVheapClear (Vheap * vheap)
{
	if (!vheap) return 0;
	vheap->count = 0;
	return 1;
}
