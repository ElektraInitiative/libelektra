#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <uv.h>

#include <kdbhelper.h>
#include <kdbio.h>
#include <kdblogger.h>

/**
 * Container for required additional information for
 * IO binding operations & libuv Handles
 */
typedef struct UvBindingData
{
	ElektraIoInterface * binding;
	union {
		ElektraIoFdOperation * fd;
		ElektraIoTimerOperation * timer;
		ElektraIoIdleOperation * idle;
	} operation;
	union {
		uv_poll_t fd;
		uv_timer_t timer;
		uv_idle_t idle;
	} handle;
} UvBindingData;

/**
 * Convert IO flags to libuv event bit mask
 * @param  flags IO flags bit mask
 * @return       libuv events bit mask
 */
static int flagsToEvents (int flags)
{
	int events = 0;
	if (flags & ELEKTRA_IO_READABLE)
	{
		events |= UV_READABLE;
	}
	if (flags & ELEKTRA_IO_WRITABLE)
	{
		events |= UV_WRITABLE;
	}
	return events;
}

/**
 * Convert libuv event bit mask to IO flags
 * @param  events libuv events bit mask
 * @return        IO flags bit mask
 */
static int eventsToFlags (int events)
{
	int flags = 0;
	if (events & UV_READABLE)
	{
		flags |= ELEKTRA_IO_READABLE;
	}
	if (events & UV_WRITABLE)
	{
		flags |= ELEKTRA_IO_WRITABLE;
	}
	return flags;
}

static UvBindingData * newBindingData (void)
{
	UvBindingData * bindingData = elektraMalloc (sizeof (*bindingData));
	if (bindingData == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}
	memset (bindingData, 0, sizeof (*bindingData));

	return bindingData;
}

/**
 * Free allocated memory after a handle has been closed.
 * Called by libuv.
 *
 * @param handle any libuv handle type (e.g. poll, timer, idle)
 */
static void ioUvBindingHandleClosedCallback (uv_handle_t * handle)
{
	assert (handle->data != NULL);
	UvBindingData * bindingData = (UvBindingData *)handle->data;

	elektraFree (bindingData);
}

/**
 * Converts the events bit mask and calls the associated callback
 * Called by libuv whenever a file descriptor status has changed.
 *
 * @param handle libuv poll handle
 * @param status 0 means no error
 * @param events events bit mask
 */
static void ioUvBindingFdCallback (uv_poll_t * handle, int status, int events)
{
	if (status != 0)
	{
		return;
	}

	assert (handle->data != NULL);
	UvBindingData * bindingData = (UvBindingData *)handle->data;
	ElektraIoFdOperation * fdOp = (ElektraIoFdOperation *)bindingData->operation.fd;

	fdOp->callback (fdOp, eventsToFlags (events));
}

/**
 * Calls the associated callback.
 * Called by libuv whenever a timer has elapsed.
 *
 * @param handle libuv timer handle
 */
#ifdef HAVE_LIBUV0
static void ioUvBindingTimerCallback (uv_timer_t * handle, int unknown ELEKTRA_UNUSED)
#endif
#ifdef HAVE_LIBUV1
	static void ioUvBindingTimerCallback (uv_timer_t * handle)
#endif
{
	assert (handle->data != NULL);
	UvBindingData * bindingData = (UvBindingData *)handle->data;
	ElektraIoTimerOperation * timerOp = (ElektraIoTimerOperation *)bindingData->operation.timer;

	timerOp->callback (timerOp);
}

/**
 * Calls the associated callback.
 * Called by libuv whenever a idle operation can perform its operations.
 *
 * @param handle libuv idle handle
 */
#ifdef HAVE_LIBUV0
static void ioUvBindingIdleCallback (uv_idle_t * handle, int unknown ELEKTRA_UNUSED)
#endif
#ifdef HAVE_LIBUV1
	static void ioUvBindingIdleCallback (uv_idle_t * handle)
#endif
{
	assert (handle->data != NULL);
	UvBindingData * bindingData = (UvBindingData *)handle->data;
	ElektraIoIdleOperation * idleOp = (ElektraIoIdleOperation *)bindingData->operation.idle;

	idleOp->callback (idleOp);
}

/**
 * Update information about a file descriptor watched by IO-Binding.
 * @see common.h ElektraIoInterface::updateFd
 */
static int ioUvBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	assert (fdOp->bindingData != NULL);
	UvBindingData * bindingData = (UvBindingData *)fdOp->bindingData;
	if (fdOp->enabled)
	{
		uv_poll_start (&bindingData->handle.fd, flagsToEvents (fdOp->flags), ioUvBindingFdCallback);
	}
	else
	{
		uv_poll_stop (&bindingData->handle.fd);
	}
	return 0;
}

/**
 * Add file descriptor to IO-Binding
 * @see common.h ElektraIoInterface::addFd
 */
static int ioUvBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp)
{
	if (fdOp->callback == NULL)
	{
		ELEKTRA_LOG_WARNING ("callback was NULL");
		return -1;
	}

	UvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return -1;
	}
	uv_loop_t * loop = (uv_loop_t *)binding->private;

	fdOp->bindingData = bindingData;
	fdOp->binding = binding;
	bindingData->operation.fd = fdOp;
	bindingData->binding = binding;
	bindingData->handle.fd.data = bindingData;

	if (uv_poll_init (loop, &bindingData->handle.fd, fdOp->fd) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize poll");
		return -1;
	}

	// Start polling if enabled
	if (fdOp->enabled)
	{
		ioUvBindingUpdateFd (fdOp);
	}

	return 0;
}

/**
 * Remove file descriptor from IO-Binding.
 * @see kdbio.h ElektraIoInterface::removeFd
 */
static int ioUvBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	assert (fdOp->bindingData != NULL);
	UvBindingData * bindingData = (UvBindingData *)fdOp->bindingData;
	uv_poll_stop (&bindingData->handle.fd);
	uv_close ((uv_handle_t *)&bindingData->handle.fd, ioUvBindingHandleClosedCallback);
	return 0;
}

/**
 * Update timer in IO-Binding.
 * @see kdbio.h ElektraIoInterface::removeFd
 */
static int ioUvBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	assert (timerOp->bindingData != NULL);
	UvBindingData * bindingData = (UvBindingData *)timerOp->bindingData;
	if (timerOp->enabled)
	{
		uv_timer_start (&bindingData->handle.timer, ioUvBindingTimerCallback, timerOp->interval, timerOp->interval);
	}
	else
	{
		uv_timer_stop (&bindingData->handle.timer);
	}
	return 0;
}

/**
 * Add timer for IO-Binding.
 * @see kdbio.h ElektraIoInterface::addTimer
 */
static int ioUvBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp)
{
	if (timerOp->callback == NULL)
	{
		ELEKTRA_LOG_WARNING ("callback was NULL");
		return -1;
	}

	UvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return -1;
	}
	uv_loop_t * loop = (uv_loop_t *)binding->private;

	timerOp->bindingData = bindingData;
	timerOp->binding = binding;
	bindingData->operation.timer = timerOp;
	bindingData->binding = binding;
	bindingData->handle.timer.data = bindingData;

	int ret = 0;
	ret = uv_timer_init (loop, &bindingData->handle.timer);
	if (ret != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize timer");
		return -1;
	}

	// Start timer if enabled
	if (timerOp->enabled)
	{
		ioUvBindingUpdateTimer (timerOp);
	}

	return 0;
}

/**
 * Remove timer from IO-Binding
 * @see kdbio.h ElektraIoInterface::removeTimer
 */
static int ioUvBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	assert (timerOp->bindingData != NULL);
	UvBindingData * bindingData = (UvBindingData *)timerOp->bindingData;
	uv_timer_stop (&bindingData->handle.timer);
	uv_close ((uv_handle_t *)&bindingData->handle.timer, ioUvBindingHandleClosedCallback);
	return 0;
}

/**
 * Update idle operation in IO-Binding
 * @see kdbio.h ElektraIoInterface::updateIdle
 */
static int ioUvBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	assert (idleOp->bindingData != NULL);
	UvBindingData * bindingData = (UvBindingData *)idleOp->bindingData;
	if (idleOp->enabled)
	{
		uv_idle_start (&bindingData->handle.idle, ioUvBindingIdleCallback);
	}
	else
	{
		uv_idle_stop (&bindingData->handle.idle);
	}
	return 0;
}

/**
 * Add idle operation to IO-Binding
 * @see kdbio.h ElektraIoInterface::addIdle
 */
static int ioUvBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp)
{
	if (idleOp->callback == NULL)
	{
		ELEKTRA_LOG_WARNING ("callback was NULL");
		return -1;
	}

	UvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return -1;
	}
	uv_loop_t * loop = (uv_loop_t *)binding->private;

	idleOp->bindingData = bindingData;
	idleOp->binding = binding;
	bindingData->operation.idle = idleOp;
	bindingData->binding = binding;
	bindingData->handle.idle.data = bindingData;

	int ret = 0;
	ret = uv_idle_init (loop, &bindingData->handle.idle);
	if (ret != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize idle");
		return -1;
	}

	// Add idle to loop if enabled
	if (idleOp->enabled)
	{
		ioUvBindingUpdateIdle (idleOp);
	}

	return 0;
}

/**
 * Remove idle operation from IO-Binding
 * @see kdbio.h ElektraIoInterface::removeIdle
 */
static int ioUvBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	assert (idleOp->bindingData != NULL);
	UvBindingData * bindingData = (UvBindingData *)idleOp->bindingData;
	uv_idle_stop (&bindingData->handle.idle);
	uv_close ((uv_handle_t *)&bindingData->handle.idle, ioUvBindingHandleClosedCallback);
	return 0;
}

/**
 * Cleanup
 * @param  binding IO-Binding
 * @see kdbio.h ElektraIoInterface::cleanup
 */
static int ioUvBindingCleanup (ElektraIoInterface * binding)
{
	assert (binding != NULL);
	elektraFree (binding);
	return 0;
}

/**
 * Create and initialize a new IO binding.
 * @param  loop Loop to use for IO operations
 * @return      Populated IO interface
 */
ElektraIoInterface * elektraIoUvNew (uv_loop_t * loop)
{
	if (loop == NULL)
	{
		ELEKTRA_LOG_WARNING ("loop was NULL");
		return NULL;
	}
	ElektraIoInterface * binding = elektraMalloc (sizeof (*binding));
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}
	// Add functions
	binding->addFd = ioUvBindingAddFd;
	binding->updateFd = ioUvBindingUpdateFd;
	binding->removeFd = ioUvBindingRemoveFd;
	binding->addTimer = ioUvBindingAddTimer;
	binding->updateTimer = ioUvBindingUpdateTimer;
	binding->removeTimer = ioUvBindingRemoveTimer;
	binding->addIdle = ioUvBindingAddIdle;
	binding->updateIdle = ioUvBindingUpdateIdle;
	binding->removeIdle = ioUvBindingRemoveIdle;
	binding->cleanup = ioUvBindingCleanup;

	// Save the libuv loop we are using
	binding->private = loop;

	return binding;
}
