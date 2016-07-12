#include "kdbprivate.h"

/**
 * Allocates the needed space and sets the stack pointer.
 *
 * @ingroup vstack
 * @param minSize the minimum size of the stack
 * @return a Vstack pointer
 * @return NULL error
 */
Vstack * elektraVstackInit (int minSize)
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
 * @return 0 on error
 * @return 1 otherwise
 */
int elektraVstackPush (Vstack * stack, void * data)
{
	if (!stack) return 0;
	// grow
	if (stack->head - stack->data >= stack->size)
	{
		stack->size <<= 1;
		// save head
		int diff = stack->head - stack->data;
		if (elektraRealloc ((void **)&stack->data, stack->size * sizeof (void *)) == -1)
		{
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
 * @return the element
 * @return NULL on empty
 */
void * elektraVstackPop (Vstack * stack)
{
	if (!stack) return NULL;
	if (elektraVstackIsEmpty (stack)) return NULL;
	stack->head--;
	// shrink
	if (stack->size > stack->minSize && stack->head - stack->data <= stack->size >> 2)
	{
		stack->size >>= 1;
		int diff = stack->head - stack->data;
		if (elektraRealloc ((void **)&stack->data, stack->size * sizeof (void *)) == -1)
		{
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
 * @return 1 on empty
 * @return 0 on non empty
 */
int elektraVstackIsEmpty (Vstack * stack)
{
	if (!stack) return 0;
	return (stack->head == stack->data);
}

/**
 * Deletes the stack, by freeing all memory.
 *
 * @ingroup vstack
 */
void elektraVstackDel (Vstack * stack)
{
	if (!stack) return;
	elektraFree (stack->data);
	elektraFree (stack);
}
