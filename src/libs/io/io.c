/**
 * @file
 *
 * @brief Implementation of I/O functions as defined in kdbio.h
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <string.h>

#include <kdbhelper.h>
#include <kdbio.h>
#include <kdbioprivate.h>
#include <kdblogger.h>

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
	memset (binding, 0, sizeof (*binding));

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
		return -1;
	}
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("fdOp cannot be NULL");
		return -1;
	}
	if (fdOp->binding != NULL)
	{
		ELEKTRA_LOG_WARNING ("operation cannot be assigned to multiple bindings");
		return -1;
	}
	fdOp->binding = binding;
	return binding->addFd (binding, fdOp);
}

int elektraIoBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("fdOp cannot be NULL");
		return -1;
	}
	return fdOp->binding->updateFd (fdOp);
}

int elektraIoBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("fdOp cannot be NULL");
		return -1;
	}
	int result = fdOp->binding->removeFd (fdOp);
	fdOp->binding = NULL;
	return result;
}

int elektraIoBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding cannot be NULL");
		return -1;
	}
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("timerOp cannot be NULL");
		return -1;
	}
	if (timerOp->binding != NULL)
	{
		ELEKTRA_LOG_WARNING ("operation cannot be assigned to multiple bindings");
		return -1;
	}
	timerOp->binding = binding;
	return binding->addTimer (binding, timerOp);
}

int elektraIoBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("timerOp cannot be NULL");
		return -1;
	}
	return timerOp->binding->updateTimer (timerOp);
}

int elektraIoBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("timerOp cannot be NULL");
		return -1;
	}
	int result = timerOp->binding->removeTimer (timerOp);
	timerOp->binding = NULL;
	return result;
}

int elektraIoBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding cannot be NULL");
		return -1;
	}
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("idleOp cannot be NULL");
		return -1;
	}
	if (idleOp->binding != NULL)
	{
		ELEKTRA_LOG_WARNING ("operation cannot be assigned to multiple bindings");
		return -1;
	}
	idleOp->binding = binding;
	return binding->addIdle (binding, idleOp);
}

int elektraIoBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("idleOp cannot be NULL");
		return -1;
	}
	return idleOp->binding->updateIdle (idleOp);
}

int elektraIoBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("idleOp cannot be NULL");
		return -1;
	}
	int result = idleOp->binding->removeIdle (idleOp);
	idleOp->binding = NULL;
	return result;
}

int elektraIoBindingCleanup (ElektraIoInterface * binding)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("binding cannot be NULL");
		return -1;
	}
	return binding->cleanup (binding);
}

void * elektraIoBindingGetData (ElektraIoInterface * binding)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return NULL;
	}

	return binding->data;
}

int elektraIoBindingSetData (ElektraIoInterface * binding, void * data)
{
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return -1;
	}

	binding->data = data;
	return 0;
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
	memset (fdOp, 0, sizeof (*fdOp));

	fdOp->fd = fd;
	fdOp->flags = flags;
	fdOp->enabled = enabled;
	fdOp->callback = callback;
	fdOp->privateData = privateData;

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
	memset (timerOp, 0, sizeof (*timerOp));

	timerOp->interval = interval;
	timerOp->enabled = enabled;
	timerOp->callback = callback;
	timerOp->privateData = privateData;

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
	memset (idleOp, 0, sizeof (*idleOp));

	idleOp->enabled = enabled;
	idleOp->callback = callback;
	idleOp->privateData = privateData;

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
		return 1;
	}

	fdOp->enabled = enabled;
	return 0;
}

int elektraIoFdSetFd (ElektraIoFdOperation * fdOp, int fd)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 1;
	}

	fdOp->fd = fd;
	return 0;
}

int elektraIoFdSetFlags (ElektraIoFdOperation * fdOp, int flags)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 1;
	}

	fdOp->flags = flags;
	return 0;
}

int elektraIoTimerSetEnabled (ElektraIoTimerOperation * timerOp, int enabled)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 1;
	}

	timerOp->enabled = enabled;
	return 0;
}

int elektraIoTimerSetInterval (ElektraIoTimerOperation * timerOp, unsigned int interval)
{
	if (timerOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 1;
	}

	timerOp->interval = interval;
	return 0;
}

int elektraIoIdleSetEnabled (ElektraIoIdleOperation * idleOp, int enabled)
{
	if (idleOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return 1;
	}

	idleOp->enabled = enabled;
	return 0;
}

// ################################
// # Getters
// ################################

int elektraIoFdGetFd (ElektraIoFdOperation * fdOp)
{
	if (fdOp == NULL)
	{
		ELEKTRA_LOG_WARNING ("operation was NULL");
		return -1;
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
		return -1;
	}

	fdOp->bindingData = data;
	return 0;
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
		return -1;
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
		return -1;
	}

	timerOp->bindingData = data;
	return 0;
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
		return -1;
	}

	idleOp->bindingData = data;
	return 0;
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
