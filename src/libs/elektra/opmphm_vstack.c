#include "kdbprivate.h"

/**
 * Allocates vstack with size specified by parameter minSize and sets the stack pointer.
 *
 * @ingroup vstack
 * @param minSize the minimum size of the stack
 * @retval a Vstack pointer
 * @retval NULL error
 */
Vstack * elektraVstackInit (size_t minSize)
{
	if (minSize < 1) return NULL;
	Vstack * newStack = elektraMalloc (sizeof (Vstack));
	if (!newStack) return NULL;
	newStack->size = minSize;
	newStack->minSize = minSize;
	newStack->data = elektraMalloc (newStack->size * sizeof (void *));
	if (!newStack->data)
	{
		elektraFree (newStack);
		return NULL;
	}
	newStack->head = newStack->data;
	return newStack;
}

/**
 * Pushes an element in the Vstack and resizes the stack if needed.
 *
 * @ingroup vstack
 * @param data the element
 * @retval 0 on error
 * @retval 1 otherwise
 */
int elektraVstackPush (Vstack * stack, void * data)
{
	if (!stack) return 0;
	// grow
	if ((size_t) (stack->head - stack->data) >= stack->size)
	{
		stack->size <<= 1;
		// save head
		int diff = stack->head - stack->data;
		if (elektraRealloc ((void **)&stack->data, stack->size * sizeof (void *)) == -1)
		{
			stack->size >>= 1;
			return 0;
		}
		// restore head
		stack->head = stack->data + diff;
	}
	*stack->head = data;
	++stack->head;
	return 1;
}

/**
 * Pops an element from the Vstack and resizes the stack if needed.
 *
 * @ingroup vstack
 * @retval the element
 * @retval NULL on empty
 */
void * elektraVstackPop (Vstack * stack)
{
	if (!stack) return NULL;
	if (elektraVstackIsEmpty (stack)) return NULL;
	--stack->head;
	// shrink
	if (stack->size > stack->minSize && (size_t) (stack->head - stack->data) <= stack->size >> 2)
	{
		stack->size >>= 1;
		int diff = stack->head - stack->data;
		if (elektraRealloc ((void **)&stack->data, stack->size * sizeof (void *)) == -1)
		{
			++stack->head;
			stack->size <<= 1;
			return NULL;
		}
		stack->head = stack->data + diff;
	}
	return *stack->head;
}

/**
 * Checks if the stack is empty.
 *
 * @ingroup vstack
 * @retval 1 on empty
 * @retval 0 on non empty
 * @retval -1 on error
 */
int elektraVstackIsEmpty (const Vstack * stack)
{
	if (!stack) return -1;
	return (stack->head == stack->data);
}

/**
 * Deletes the stack, by freeing all memory.
 * @ingroup vstack
 *
 */
void elektraVstackDel (Vstack * stack)
{
	if (!stack) return;
	elektraFree (stack->data);
	elektraFree (stack);
}

/**
 * Clears the stack in a fast fashion.
 * Resets the stackpointer to the base and does not reallocate memory.
 * The next reallocation happens at Pop.
 *
 * @ingroup vstack
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraVstackClear (Vstack * stack)
{
	if (!stack) return 0;
	stack->head = stack->data;
	return 1;
}
