#include <kdbchangetracking.h>
#include <kdbprivate.h>

/**
 * Returns the changetracking context of the given KDB instance
 *
 * @param kdb the KDB instance
 * @return ChangeTrackingContext or @p NULL if @p kdb is NULL
 */
const ChangeTrackingContext * elektraChangeTrackingGetContextFromKdb (KDB * kdb)
{
	if (kdb == NULL)
	{
		return NULL;
	}

	return &kdb->changeTrackingContext;
}

/**
 * Returns the changetracking context for the KDB instance associated with the specified plugin
 *
 * @param plugin the plugin
 * @return ChangeTrackingContext or @p NULL if @p plugin is NULL or does not have a @p KDB instance associated with it
 */
const ChangeTrackingContext * elektraChangeTrackingGetContextFromPlugin (Plugin * plugin)
{
	if (plugin->global == NULL)
	{
		return NULL;
	}

	Key * kdbKey = ksLookupByName (plugin->global, "system:/elektra/kdb", 0);
	if (kdbKey == NULL)
	{
		return NULL;
	}

	const void * kdbPtr = keyValue (kdbKey);
	KDB * kdb = kdbPtr == NULL ? NULL : *(KDB **) keyValue (kdbKey);

	return elektraChangeTrackingGetContextFromKdb (kdb);
}

/**
 * @internal
 *
 * @brief For testing purposes only: Create a ChangeTrackingContext from a keyset
 *
 * @param oldKeys the old keys
 * @return a new changetracking context
 */
ChangeTrackingContext * elektraChangeTrackingCreateContextForTesting (KeySet * oldKeys)
{
	ChangeTrackingContext * context = elektraCalloc (sizeof (ChangeTrackingContext));

	ksIncRef (oldKeys);
	context->oldKeys = oldKeys;

	return context;
}

/**
 * @internal
 *
 * @brief For testing purposes only: Delete a ChangeTrackingContext object
 *
 * @param context the object to delete
 */
void elektraChangeTrackingContextDel (ChangeTrackingContext * context)
{
	if (context->oldKeys != NULL)
	{
		ksDecRef (context->oldKeys);
		ksDel (context->oldKeys);
		context->oldKeys = NULL;
	}

	elektraFree (context);
}

/**
 * Calculates the changes between the provided KeySet and the given ChangeTrackingContext.
 *
 * @param newKeys the KeySet
 * @param context the ChangeTrackingContext
 * @param parentKey if not @p NULL, only keys below or same of this key are considered
 * @return the changes between @p newKeys and @p context OR @p NULL if either one of them is @p NULL
 */
ElektraDiff * elektraChangeTrackingCalculateDiff (KeySet * newKeys, const ChangeTrackingContext * context, Key * parentKey)
{
	if (newKeys == NULL || context == NULL)
	{
		return NULL;
	}

	return elektraDiffCalculate (newKeys, context->oldKeys, parentKey);
}
