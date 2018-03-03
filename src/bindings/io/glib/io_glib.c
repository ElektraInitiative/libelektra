/**
 * @file
 *
 * @brief I/O UV binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdbio.h>
#include <kdblogger.h>

#include <stdio.h> // printf()

/**
 * Container for required additional information for
 * I/O binding operations & libuv Handles
 */
typedef struct
{
	union {
		ElektraIoFdOperation * fd;
		ElektraIoTimerOperation * timer;
		ElektraIoIdleOperation * idle;
	} operation;
	GSource * source;
	int enabled;
} GlibBindingData;

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
		events |= G_IO_IN;
	}
	if (flags & ELEKTRA_IO_WRITABLE)
	{
		events |= G_IO_OUT;
	}
	return events;
}

/**
 * Convert libuv event bit mask to I/O flags
 * @param  events libuv events bit mask
 * @return        I/O flags bit mask
 */
static int eventsToFlags (int events)
{
	int flags = 0;
	if (events & G_IO_IN)
	{
		flags |= ELEKTRA_IO_READABLE;
	}
	if (events & G_IO_OUT)
	{
		flags |= ELEKTRA_IO_WRITABLE;
	}
	return flags;
}

static GlibBindingData * newBindingData (void)
{
	GlibBindingData * bindingData = elektraCalloc (sizeof (*bindingData));
	if (bindingData == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}

	return bindingData;
}

/**
 * Free allocated memory after a handle has been closed.
 * Called by libuv.
 *
 * @param handle any libuv handle type (e.g. poll, timer, idle)
 */
/*static void ioGlibBindingHandleClosedCallback (uv_handle_t * handle)
{
	ELEKTRA_NOT_NULL (handle->data);
	GlibBindingData * bindingData = (GlibBindingData *)handle->data;

	elektraFree (bindingData);
}*/

/**
 * Converts the events bit mask and calls the associated callback
 * Called by libuv whenever a file descriptor status has changed.
 *
 * @param handle libuv poll handle
 * @param status 0 means no error
 * @param events events bit mask
 */
static void ioGlibBindingFdCallback (void)
{
	// TODO in fd_dispatch erledigen
	/*if (status != 0)
	{
		return;
	}

	ELEKTRA_NOT_NULL (handle->data);
	GlibBindingData * bindingData = (GlibBindingData *)handle->data;
	ElektraIoFdOperation * fdOp = (ElektraIoFdOperation *)bindingData->operation.fd;

	elektraIoFdGetCallback (fdOp) (fdOp, eventsToFlags (events));*/
}

/**
 * Calls the associated callback.
 * Called by libuv whenever a timer has elapsed.
 *
 * @param handle libuv timer handle
 */
static void ioGlibBindingTimerCallback (void * data)
{
	/*ELEKTRA_NOT_NULL (handle->data);
	GlibBindingData * bindingData = (GlibBindingData *)handle->data;
	ElektraIoTimerOperation * timerOp = (ElektraIoTimerOperation *)bindingData->operation.timer;

	elektraIoTimerGetCallback (timerOp) (timerOp);*/
}

/**
 * Calls the associated callback.
 * Called by libuv whenever a idle operation can perform its operations.
 *
 * @param handle libuv idle handle
 */
static int ioGlibBindingIdleCallback (void * data)
{
	ELEKTRA_NOT_NULL (data);
	GlibBindingData * bindingData = (GlibBindingData *)data;
	ElektraIoIdleOperation * idleOp = (ElektraIoIdleOperation *)bindingData->operation.idle;

	if (bindingData->enabled)
	{
		elektraIoIdleGetCallback (idleOp) (idleOp);
	}

	return G_SOURCE_CONTINUE;
}

/**
 * Update information about a file descriptor watched by I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateFd
 */
static int ioGlibBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	/*ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	GlibBindingData * bindingData = (GlibBindingData *)elektraIoFdGetBindingData (fdOp);
	if (elektraIoFdIsEnabled (fdOp))
	{
		// uv_poll_start (&bindingData->handle.fd, flagsToEvents (elektraIoFdGetFlags (fdOp)), ioGlibBindingFdCallback);
	}
	else
	{
		// uv_poll_stop (&bindingData->handle.fd);
	}
	return 1;*/
}

/**
 * Add file descriptor to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddFd
 */
static int ioGlibBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp)
{
	/*GlibBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	// uv_loop_t * loop = (uv_loop_t *)elektraIoBindingGetData (binding);
	ELEKTRA_NOT_NULL (loop);

	elektraIoFdSetBindingData (fdOp, bindingData);
	bindingData->operation.fd = fdOp;
	bindingData->handle.fd.data = bindingData;

	if (uv_poll_init (loop, &bindingData->handle.fd, elektraIoFdGetFd (fdOp)) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize poll");
		return 0;
	}

	// Start polling if enabled
	if (elektraIoFdIsEnabled (fdOp))
	{
		ioGlibBindingUpdateFd (fdOp);
	}

	return 1;*/
}

/**
 * Remove file descriptor from I/O binding.
 * @see kdbio.h ::ElektraIoBindingRemoveFd
 */
static int ioGlibBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	/*ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	GlibBindingData * bindingData = (GlibBindingData *)elektraIoFdGetBindingData (fdOp);
	uv_poll_stop (&bindingData->handle.fd);
	uv_close ((uv_handle_t *)&bindingData->handle.fd, ioGlibBindingHandleClosedCallback);
	return 1;*/
}

/**
 * Update timer in I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateTimer
 */
static int ioGlibBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	/*ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	GlibBindingData * bindingData = (GlibBindingData *)elektraIoTimerGetBindingData (timerOp);
	if (elektraIoTimerIsEnabled (timerOp))
	{
		unsigned int interval = elektraIoTimerGetInterval (timerOp);
		uv_timer_start (&bindingData->handle.timer, ioGlibBindingTimerCallback, interval, interval);
	}
	else
	{
		uv_timer_stop (&bindingData->handle.timer);
	}
	return 1;*/
}

/**
 * Add timer for I/O binding.
 * @see kdbio.h ::ElektraIoBindingAddTimer
 */
static int ioGlibBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp)
{
	/*GlibBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	uv_loop_t * loop = (uv_loop_t *)elektraIoBindingGetData (binding);
	ELEKTRA_NOT_NULL (loop);

	elektraIoTimerSetBindingData (timerOp, bindingData);
	bindingData->operation.timer = timerOp;
	bindingData->handle.timer.data = bindingData;

	int ret = 0;
	ret = uv_timer_init (loop, &bindingData->handle.timer);
	if (ret != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize timer");
		return 0;
	}

	// Start timer if enabled
	if (elektraIoTimerIsEnabled (timerOp))
	{
		ioGlibBindingUpdateTimer (timerOp);
	}

	return 1;*/
}

/**
 * Remove timer from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveTimer
 */
static int ioGlibBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	/*ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	GlibBindingData * bindingData = (GlibBindingData *)elektraIoTimerGetBindingData (timerOp);
	uv_timer_stop (&bindingData->handle.timer);
	uv_close ((uv_handle_t *)&bindingData->handle.timer, ioGlibBindingHandleClosedCallback);
	return 1;*/
}

/**
 * Update idle operation in I/O binding
 * @see kdbio.h ::ElektraIoBindingUpdateIdle
 */
static int ioGlibBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	ELEKTRA_NOT_NULL (elektraIoIdleGetBindingData (idleOp));
	GlibBindingData * bindingData = (GlibBindingData *)elektraIoIdleGetBindingData (idleOp);
	bindingData->enabled = elektraIoIdleIsEnabled (idleOp);
	return 1;
}

/**
 * Add idle operation to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddIdle
 */
static int ioGlibBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp)
{
	GlibBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	GMainContext * context = (GMainContext *)elektraIoBindingGetData (binding);
	ELEKTRA_NOT_NULL (context);

	elektraIoIdleSetBindingData (idleOp, bindingData);
	bindingData->operation.idle = idleOp;

	GSource * idleSource = g_idle_source_new ();
	g_source_set_callback (idleSource, ioGlibBindingIdleCallback, bindingData, NULL);
	g_source_attach (idleSource, context);

	bindingData->source = idleSource;

	ioGlibBindingUpdateIdle (idleOp);

	return 1;
}

/**
 * Remove idle operation from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveIdle
 */
static int ioGlibBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	ELEKTRA_NOT_NULL (elektraIoIdleGetBindingData (idleOp));
	GlibBindingData * bindingData = (GlibBindingData *)elektraIoIdleGetBindingData (idleOp);

	g_source_destroy (bindingData->source);
	g_source_unref (bindingData->source);
	elektraFree (bindingData);

	return 1;
}

/**
 * Cleanup
 * @param  binding I/O binding
 * @see kdbio.h ::ElektraIoBindingCleanup
 */
static int ioGlibBindingCleanup (ElektraIoInterface * binding)
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
ElektraIoInterface * elektraIoGlibNew (GMainContext * context)
{
	// Initialize I/O interface
	ElektraIoInterface * binding = elektraIoNewBinding (
		// file descriptors
		ioGlibBindingAddFd, ioGlibBindingUpdateFd, ioGlibBindingRemoveFd,
		// timers
		ioGlibBindingAddTimer, ioGlibBindingUpdateTimer, ioGlibBindingRemoveTimer,
		// idle
		ioGlibBindingAddIdle, ioGlibBindingUpdateIdle, ioGlibBindingRemoveIdle,
		// cleanup
		ioGlibBindingCleanup);
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraIoNewBinding failed");
		return NULL;
	}

	// Save the glib context we are using
	elektraIoBindingSetData (binding, context);

	return binding;
}
