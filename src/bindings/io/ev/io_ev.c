/**
 * @file
 *
 * @brief I/O EV binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#include <stdlib.h>

#include <ev.h>

#include <kdbassert.h>
#include <elektra/kdbhelper.h>
#include <elektra/kdbio.h>
#include <kdblogger.h>

typedef struct ev_loop ev_loop_t;

/**
 * Container for required additional information for
 * I/O binding operations & libev Handles
 */
typedef struct EvBindingData
{
	union
	{
		ElektraIoFdOperation * fd;
		ElektraIoTimerOperation * timer;
		ElektraIoIdleOperation * idle;
	} operation;
	union
	{
		ev_io fd;
		ev_timer timer;
		ev_idle idle;
	} handle;
} EvBindingData;

/**
 * Convert I/O flags to libuv event bit mask
 * @param  flags I/O flags bit mask
 * @return       libuv events bit mask
 */
static int flagsToEvents (int flags)
{
	int events = 0;
	if (flags & ELEKTRA_IO_READABLE)
	{
		events |= EV_READ;
	}
	if (flags & ELEKTRA_IO_WRITABLE)
	{
		events |= EV_WRITE;
	}
	return events;
}

/**
 * Convert libuv event bit mask to I/O flags
 * @param  events libev events bit mask
 * @return        I/O flags bit mask
 */
static int eventsToFlags (int events)
{
	int flags = 0;
	if (events & EV_READ)
	{
		flags |= ELEKTRA_IO_READABLE;
	}
	if (events & EV_WRITE)
	{
		flags |= ELEKTRA_IO_WRITABLE;
	}
	return flags;
}

static EvBindingData * newBindingData (void)
{
	EvBindingData * bindingData = elektraCalloc (sizeof (*bindingData));
	if (bindingData == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraCalloc failed");
		return NULL;
	}

	return bindingData;
}

/**
 * Converts the events bit mask and calls the associated callback
 * Called by libuv whenever a file descriptor status has changed.
 *
 * @param handle libuv poll handle
 * @param events events bit mask
 */
static void ioEvBindingFdCallback (ev_loop_t * loop ELEKTRA_UNUSED, ev_io * handle, int revents ELEKTRA_UNUSED)
{
	ELEKTRA_NOT_NULL (handle->data);
	EvBindingData * bindingData = (EvBindingData *) handle->data;
	ElektraIoFdOperation * fdOp = (ElektraIoFdOperation *) bindingData->operation.fd;

	elektraIoFdGetCallback (fdOp) (fdOp, eventsToFlags (revents));
}

/**
 * Calls the associated callback.
 * Called by libuv whenever a timer has elapsed.
 *
 * @param handle libuv timer handle
 */
static void ioEvBindingTimerCallback (ev_loop_t * loop ELEKTRA_UNUSED, ev_timer * handle, int revents ELEKTRA_UNUSED)
{
	ELEKTRA_NOT_NULL (handle->data);
	EvBindingData * bindingData = (EvBindingData *) handle->data;
	ElektraIoTimerOperation * timerOp = (ElektraIoTimerOperation *) bindingData->operation.timer;

	elektraIoTimerGetCallback (timerOp) (timerOp);
}

/**
 * Calls the associated callback.
 * Called by libuv whenever an idle operation can perform its operations.
 *
 * @param handle libuv idle handle
 */
static void ioEvBindingIdleCallback (ev_loop_t * loop ELEKTRA_UNUSED, ev_idle * handle, int revents ELEKTRA_UNUSED)
{
	ELEKTRA_NOT_NULL (handle->data);
	EvBindingData * bindingData = (EvBindingData *) handle->data;
	ElektraIoIdleOperation * idleOp = (ElektraIoIdleOperation *) bindingData->operation.idle;

	elektraIoIdleGetCallback (idleOp) (idleOp);
}

/**
 * Update information about a file descriptor watched by I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateFd
 */
static int ioEvBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	EvBindingData * bindingData = (EvBindingData *) elektraIoFdGetBindingData (fdOp);
	ELEKTRA_NOT_NULL (bindingData);
	ev_loop_t * loop = elektraIoBindingGetData (elektraIoFdGetBinding (fdOp));
	ev_io * fd = &bindingData->handle.fd;
	if (elektraIoFdIsEnabled (fdOp))
	{
		ev_io_stop (loop, fd);
		ev_io_set (fd, elektraIoFdGetFd (fdOp), flagsToEvents (elektraIoFdGetFlags (fdOp)));
		ev_io_start (loop, fd);
	}
	else
	{
		ev_io_stop (loop, fd);
	}
	return 1;
}

/**
 * Add file descriptor to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddFd
 */
static int ioEvBindingAddFd (ElektraIoInterface * binding ELEKTRA_UNUSED, ElektraIoFdOperation * fdOp)
{
	EvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	elektraIoFdSetBindingData (fdOp, bindingData);
	bindingData->operation.fd = fdOp;
	bindingData->handle.fd.data = bindingData;

	ev_io * fd = &bindingData->handle.fd;
	ev_io_init (fd, ioEvBindingFdCallback, elektraIoFdGetFd (fdOp), flagsToEvents (elektraIoFdGetFlags (fdOp)));

	// Start polling if enabled
	if (elektraIoFdIsEnabled (fdOp))
	{
		ioEvBindingUpdateFd (fdOp);
	}

	return 1;
}

/**
 * Remove file descriptor from I/O binding.
 * @see kdbio.h ::ElektraIoBindingRemoveFd
 */
static int ioEvBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	EvBindingData * bindingData = (EvBindingData *) elektraIoFdGetBindingData (fdOp);
	ELEKTRA_NOT_NULL (bindingData);
	ev_loop_t * loop = elektraIoBindingGetData (elektraIoFdGetBinding (fdOp));
	ev_io * fd = &bindingData->handle.fd;
	ev_io_stop (loop, fd);
	elektraFree (bindingData);
	return 1;
}

/**
 * Update timer in I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateTimer
 */
static int ioEvBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	EvBindingData * bindingData = (EvBindingData *) elektraIoTimerGetBindingData (timerOp);
	ELEKTRA_NOT_NULL (bindingData);
	ev_loop_t * loop = elektraIoBindingGetData (elektraIoTimerGetBinding (timerOp));
	ev_timer * timer = &bindingData->handle.timer;
	if (elektraIoTimerIsEnabled (timerOp))
	{
		double interval = elektraIoTimerGetInterval (timerOp) / 1000.0;
		ev_timer_set (timer, interval, interval);
		ev_timer_start (loop, timer);
	}
	else
	{
		ev_timer_stop (loop, timer);
	}
	return 1;
}

/**
 * Add timer for I/O binding.
 * @see kdbio.h ::ElektraIoBindingAddTimer
 */
static int ioEvBindingAddTimer (ElektraIoInterface * binding ELEKTRA_UNUSED, ElektraIoTimerOperation * timerOp)
{
	EvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	elektraIoTimerSetBindingData (timerOp, bindingData);
	bindingData->operation.timer = timerOp;
	bindingData->handle.timer.data = bindingData;

	ev_timer * timer = &bindingData->handle.timer;
	ev_timer_init (timer, ioEvBindingTimerCallback, 0, 0);

	// Start timer if enabled
	if (elektraIoTimerIsEnabled (timerOp))
	{
		ioEvBindingUpdateTimer (timerOp);
	}

	return 1;
}

/**
 * Remove timer from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveTimer
 */
static int ioEvBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	EvBindingData * bindingData = (EvBindingData *) elektraIoTimerGetBindingData (timerOp);
	ELEKTRA_NOT_NULL (bindingData);
	ev_loop_t * loop = elektraIoBindingGetData (elektraIoTimerGetBinding (timerOp));
	ev_timer * timer = &bindingData->handle.timer;
	ev_timer_stop (loop, timer);
	elektraFree (bindingData);
	return 1;
}

/**
 * Update idle operation in I/O binding
 * @see kdbio.h ::ElektraIoBindingUpdateIdle
 */
static int ioEvBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	EvBindingData * bindingData = elektraIoIdleGetBindingData (idleOp);
	ELEKTRA_NOT_NULL (bindingData);
	ev_loop_t * loop = elektraIoBindingGetData (elektraIoIdleGetBinding (idleOp));
	ev_idle * idle = &bindingData->handle.idle;
	if (elektraIoIdleIsEnabled (idleOp))
	{
		ev_idle_start (loop, idle);
	}
	else
	{
		ev_idle_stop (loop, idle);
	}
	return 1;
}

/**
 * Add idle operation to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddIdle
 */
static int ioEvBindingAddIdle (ElektraIoInterface * binding ELEKTRA_UNUSED, ElektraIoIdleOperation * idleOp)
{
	EvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	elektraIoIdleSetBindingData (idleOp, bindingData);
	bindingData->operation.idle = idleOp;
	bindingData->handle.idle.data = bindingData;

	ev_idle * idle = &bindingData->handle.idle;
	ev_idle_init (idle, ioEvBindingIdleCallback);

	// Add idle to loop if enabled
	if (elektraIoIdleIsEnabled (idleOp))
	{
		ioEvBindingUpdateIdle (idleOp);
	}

	return 1;
}

/**
 * Remove idle operation from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveIdle
 */
static int ioEvBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	EvBindingData * bindingData = elektraIoIdleGetBindingData (idleOp);
	ELEKTRA_NOT_NULL (bindingData);
	ev_loop_t * loop = elektraIoBindingGetData (elektraIoIdleGetBinding (idleOp));
	ev_idle * idle = &bindingData->handle.idle;
	ev_idle_stop (loop, idle);
	elektraFree (bindingData);
	return 1;
}

/**
 * Cleanup
 * @param  binding I/O binding
 * @see kdbio.h ::ElektraIoBindingCleanup
 */
static int ioEvBindingCleanup (ElektraIoInterface * binding)
{
	ELEKTRA_NOT_NULL (binding);
	elektraFree (binding);
	return 1;
}

/**
 * Create and initialize a new I/O binding.
 * @param  loop Loop to use for I/O operations
 * @return      Populated I/O interface
 */
ElektraIoInterface * elektraIoEvNew (struct ev_loop * loop)
{
	if (loop == NULL)
	{
		ELEKTRA_LOG_WARNING ("loop was NULL");
		return NULL;
	}
	// Initialize I/O interface
	ElektraIoInterface * binding = elektraIoNewBinding (
		// file descriptors
		ioEvBindingAddFd, ioEvBindingUpdateFd, ioEvBindingRemoveFd,
		// timers
		ioEvBindingAddTimer, ioEvBindingUpdateTimer, ioEvBindingRemoveTimer,
		// idle
		ioEvBindingAddIdle, ioEvBindingUpdateIdle, ioEvBindingRemoveIdle,
		// cleanup
		ioEvBindingCleanup);
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraIoNewBinding failed");
		return NULL;
	}

	// Save the libuv loop we are using
	elektraIoBindingSetData (binding, loop);

	return binding;
}
