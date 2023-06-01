/**
 * @file
 *
 * @brief Implementation of I/O functions as defined in kdbio.h
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/io/api.h>
#include <elektra/io/plugin.h>
#include <elektra/plugin/invoke.h>
#include <fcntl.h>
#include <internal/io/private.h>
#include <internal/kdbprivate.h>
#include <internal/utility/alloc.h>
#include <internal/utility/logger.h>
#include <stdio.h>

int elektraIoContract (KeySet * contract, ElektraIoInterface * ioBinding)
{
	if (contract == NULL || ioBinding == NULL) return -1;

	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/io/binding", KEY_BINARY, KEY_SIZE, sizeof (ioBinding),
				       KEY_VALUE, &ioBinding, KEY_END));

	return 0;
}

ElektraIoInterface * elektraIoGetBinding (KDB * kdb)
{
	Key * ioBindingKey = ksLookupByName (kdb->global, "system:/elektra/io/binding", 0);
	const void * bindingPtr = keyValue (ioBindingKey);
	ElektraIoInterface * binding = bindingPtr == NULL ? NULL : *(ElektraIoInterface **) keyValue (ioBindingKey);
	return binding;
}

// ################################
// # Binding accessors
// ################################

ElektraIoInterface * elektraIoNewBinding (ElektraIoBindingAddFd * addFd, ElektraIoBindingUpdateFd * updateFd,
					  ElektraIoBindingRemoveFd * removeFd, ElektraIoBindingAddTimer * addTimer,
					  ElektraIoBindingUpdateTimer * updateTimer, ElektraIoBindingRemoveTimer * removeTimer,
					  ElektraIoBindingAddIdle * addIdle, ElektraIoBindingUpdateIdle * updateIdle,
					  ElektraIoBindingRemoveIdle * removeIdle, ElektraIoBindingCleanup * cleanup)
{
	if (addFd == NULL)
	{
		ELEKTRA_LOG_WARNING ("addFd cannot be NULL");
		return NULL;
	}
	if (updateFd == NULL)
	{
		ELEKTRA_LOG_WARNING ("updateFd cannot be NULL");
		return NULL;
	}
	if (removeFd == NULL)
	{
		ELEKTRA_LOG_WARNING ("removeFd cannot be NULL");
		return NULL;
	}
	if (addTimer == NULL)
	{
		ELEKTRA_LOG_WARNING ("addTimer cannot be NULL");
		return NULL;
	}
	if (updateTimer == NULL)
	{
		ELEKTRA_LOG_WARNING ("updateTimer cannot be NULL");
		return NULL;
	}
	if (removeTimer == NULL)
	{
		ELEKTRA_LOG_WARNING ("removeTimer cannot be NULL");
		return NULL;
	}
	if (addIdle == NULL)
	{
		ELEKTRA_LOG_WARNING ("addIdle cannot be NULL");
		return NULL;
	}
	if (updateIdle == NULL)
	{
		ELEKTRA_LOG_WARNING ("updateIdle cannot be NULL");
		return NULL;
	}
	if (removeIdle == NULL)
	{
		ELEKTRA_LOG_WARNING ("removeIdle cannot be NULL");
		return NULL;
	}
	if (cleanup == NULL)
	{
		ELEKTRA_LOG_WARNING ("cleanup cannot be NULL");
		return NULL;
	}

	ElektraIoInterface * binding = elektraMalloc (sizeof (*binding));
	if (!binding)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}

	binding->data = NULL;
	binding->addFd = addFd;
	binding->updateFd = updateFd;
	binding->removeFd = removeFd;
	binding->addTimer = addTimer;
	binding->updateTimer = updateTimer;
	binding->removeTimer = removeTimer;
	binding->addIdle = addIdle;
	binding->updateIdle = updateIdle;
	binding->removeIdle = removeIdle;
	binding->cleanup = cleanup;

	return binding;
}

int elektraIoBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding cannot be NULL");
		return 0;
	}
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("fdOp cannot be NULL");
		return 0;
	}
	if (fdOp->binding != NULL)
	{
		ELEKTRA_LOG_WARNING ("operation cannot be assigned to multiple bindings");
		return 0;
	}
	fdOp->binding = binding;
	int result = binding->addFd (binding, fdOp);
	if (!result)
	{
		fdOp->binding = NULL;
	}
	return result;
}

int elektraIoBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("fdOp cannot be NULL");
		return 0;
	}
	return fdOp->binding->updateFd (fdOp);
}

int elektraIoBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("fdOp cannot be NULL");
		return 0;
	}
	int result = fdOp->binding->removeFd (fdOp);
	if (result)
	{
		fdOp->binding = NULL;
	}
	return result;
}

int elektraIoBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding cannot be NULL");
		return 0;
	}
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("timerOp cannot be NULL");
		return 0;
	}
	if (timerOp->binding != NULL)
	{
		ELEKTRA_LOG_WARNING ("operation cannot be assigned to multiple bindings");
		return 0;
	}
	timerOp->binding = binding;
	int result = binding->addTimer (binding, timerOp);
	if (!result)
	{
		timerOp->binding = NULL;
	}
	return result;
}

int elektraIoBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("timerOp cannot be NULL");
		return 0;
	}
	return timerOp->binding->updateTimer (timerOp);
}

int elektraIoBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("timerOp cannot be NULL");
		return 0;
	}
	int result = timerOp->binding->removeTimer (timerOp);
	if (result)
	{
		timerOp->binding = NULL;
	}
	return result;
}

int elektraIoBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding cannot be NULL");
		return 0;
	}
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("idleOp cannot be NULL");
		return 0;
	}
	if (idleOp->binding != NULL)
	{
		ELEKTRA_LOG_WARNING ("operation cannot be assigned to multiple bindings");
		return 0;
	}
	idleOp->binding = binding;
	int result = binding->addIdle (binding, idleOp);
	if (!result)
	{
		idleOp->binding = NULL;
	}
	return result;
}

int elektraIoBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("idleOp cannot be NULL");
		return 0;
	}
	return idleOp->binding->updateIdle (idleOp);
}

int elektraIoBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("idleOp cannot be NULL");
		return 0;
	}
	int result = idleOp->binding->removeIdle (idleOp);
	if (result)
	{
		idleOp->binding = NULL;
	}
	return result;
}

int elektraIoBindingCleanup (ElektraIoInterface * binding)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding cannot be NULL");
		return 0;
	}
	return binding->cleanup (binding);
}

void * elektraIoBindingGetData (ElektraIoInterface * binding)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding was NULL");
		return NULL;
	}

	return binding->data;
}

int elektraIoBindingSetData (ElektraIoInterface * binding, void * data)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	binding->data = data;
	return 1;
}


// ################################
// # Constructors for Operations
// ################################

ElektraIoFdOperation * elektraIoNewFdOperation (int fd, int flags, int enabled, ElektraIoFdCallback callback, void * privateData)
{
	if (callback == NULL)
	{
		ELEKTRA_LOG_WARNING ("callback cannot be NULL");
		return NULL;
	}

	ElektraIoFdOperation * fdOp = elektraMalloc (sizeof (*fdOp));
	if (!fdOp)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}

	fdOp->fd = fd;
	fdOp->flags = flags;
	fdOp->enabled = enabled;
	fdOp->callback = callback;
	fdOp->privateData = privateData;
	fdOp->binding = NULL;
	fdOp->bindingData = NULL;

	return fdOp;
}

ElektraIoTimerOperation * elektraIoNewTimerOperation (unsigned int interval, int enabled, ElektraIoTimerCallback callback,
						      void * privateData)
{
	if (callback == NULL)
	{
		ELEKTRA_LOG_WARNING ("callback cannot be NULL");
		return NULL;
	}

	ElektraIoTimerOperation * timerOp = elektraMalloc (sizeof (*timerOp));
	if (!timerOp)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}

	timerOp->interval = interval;
	timerOp->enabled = enabled;
	timerOp->callback = callback;
	timerOp->privateData = privateData;
	timerOp->binding = NULL;
	timerOp->bindingData = NULL;

	return timerOp;
}

ElektraIoIdleOperation * elektraIoNewIdleOperation (int enabled, ElektraIoIdleCallback callback, void * privateData)
{
	if (callback == NULL)
	{
		ELEKTRA_LOG_WARNING ("callback cannot be NULL");
		return NULL;
	}

	ElektraIoIdleOperation * idleOp = elektraMalloc (sizeof (*idleOp));
	if (!idleOp)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}

	idleOp->enabled = enabled;
	idleOp->callback = callback;
	idleOp->privateData = privateData;
	idleOp->binding = NULL;
	idleOp->bindingData = NULL;

	return idleOp;
}


// ################################
// # Setters
// ################################

int elektraIoFdSetEnabled (ElektraIoFdOperation * fdOp, int enabled)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	fdOp->enabled = enabled;
	return 1;
}

int elektraIoFdSetFlags (ElektraIoFdOperation * fdOp, int flags)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}
	int rdwrFlags = flags & (O_RDONLY | O_WRONLY | O_RDWR);
	// `open(2)` requires exactly one of these:
	if (!(rdwrFlags == O_RDONLY || rdwrFlags == O_WRONLY || rdwrFlags == O_RDWR))
	{
		ELEKTRA_LOG_NOTICE ("file flags must be exactly one of: read only, write only or read write. actual flag is: %0d", flags);
		return -1;
	}
	// since custom flags are allowed by `fcntl.h`, no further checks are required
	fdOp->flags = flags;
	return 1;
}

int elektraIoTimerSetEnabled (ElektraIoTimerOperation * timerOp, int enabled)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	timerOp->enabled = enabled;
	return 1;
}

int elektraIoTimerSetInterval (ElektraIoTimerOperation * timerOp, unsigned int interval)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	timerOp->interval = interval;
	return 1;
}

int elektraIoIdleSetEnabled (ElektraIoIdleOperation * idleOp, int enabled)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	idleOp->enabled = enabled;
	return 1;
}

// ################################
// # Getters
// ################################

int elektraIoFdGetFd (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	return fdOp->fd;
}

void * elektraIoFdGetData (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return fdOp->privateData;
}

int elektraIoFdSetBindingData (ElektraIoFdOperation * fdOp, void * data)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	fdOp->bindingData = data;
	return 1;
}

void * elektraIoFdGetBindingData (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return fdOp->bindingData;
}

ElektraIoInterface * elektraIoFdGetBinding (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return fdOp->binding;
}

int elektraIoFdIsEnabled (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	return fdOp->enabled;
}

int elektraIoFdGetFlags (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	return fdOp->flags;
}

ElektraIoFdCallback elektraIoFdGetCallback (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return fdOp->callback;
}

int elektraIoTimerSetBindingData (ElektraIoTimerOperation * timerOp, void * data)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	timerOp->bindingData = data;
	return 1;
}

void * elektraIoTimerGetBindingData (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return timerOp->bindingData;
}

ElektraIoInterface * elektraIoTimerGetBinding (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return timerOp->binding;
}

void * elektraIoTimerGetData (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return timerOp->privateData;
}

int elektraIoTimerIsEnabled (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	return timerOp->enabled;
}

unsigned int elektraIoTimerGetInterval (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	return timerOp->interval;
}

ElektraIoTimerCallback elektraIoTimerGetCallback (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return timerOp->callback;
}

int elektraIoIdleSetBindingData (ElektraIoIdleOperation * idleOp, void * data)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	idleOp->bindingData = data;
	return 1;
}

void * elektraIoIdleGetBindingData (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return idleOp->bindingData;
}

ElektraIoInterface * elektraIoIdleGetBinding (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return idleOp->binding;
}

void * elektraIoIdleGetData (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return idleOp->privateData;
}

int elektraIoIdleIsEnabled (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 0;
	}

	return idleOp->enabled;
}

ElektraIoIdleCallback elektraIoIdleGetCallback (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return idleOp->callback;
}
