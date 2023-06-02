/**
 * @file
 *
 * @brief I/O glib binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#include <stdlib.h>

#include <glib.h>

#include <elektra/io/api.h>

#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>

typedef struct _GlibBindingData GlibBindingData;

/**
 * Custom glib source for fd operations.
 *
 * @internal
 */
typedef struct
{
	GSource gSource;	       /*!< FdSource and GSource start at the same address */
	GPollFD pollFd;		       /*!< polling information: includes file descriptor and flags */
	int isPolling;		       /*!< indicates wheter source is currently polling */
	GlibBindingData * bindingData; /*!< backreference to binding data */
} FdSource;


/**
 * Container for required additional information for
 * I/O binding operations & glib sources
 *
 * @internal
 */
struct _GlibBindingData
{
	union
	{
		ElektraIoFdOperation * fd;
		ElektraIoTimerOperation * timer;
		ElektraIoIdleOperation * idle;
	} operation;
	union
	{
		GSource * gSource;
		FdSource * fdSource;
	} source;

	int enabled; /*!< operation's enabled flag. Only updated on elektraIoBindingUpdateOPERATION */
	union
	{
		unsigned int timerInterval; /*!< timer interval. Only updated on elektraIoBindingUpdateTimer */
	} cache;
	GSourceFuncs * fdFuncs; /*!< functions for fd source */
};

/**
 * Convert I/O flags to glib event bit mask
 * @param  flags I/O flags bit mask
 * @return       glib events bit mask
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
 * Convert glib event bit mask to I/O flags
 * @param  events glib events bit mask
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
		ELEKTRA_LOG_WARNING ("elektraCalloc failed");
		return NULL;
	}

	return bindingData;
}

/**
 * Prepare the file descriptor source before poll() is called.
 *
 * @param  source  glib source
 * @param  timeout allows to set a timeout for poll()
 * @return         G_SOURCE_CONTINUE if I/O operation is enabled and events are present
 */
static int ioGlibBindingFdPrepare (GSource * source, gint * timeout)
{
	FdSource * fdSource = (FdSource *) source;
	*timeout = -1; // we can wait until data is available

	int enabled = fdSource->bindingData->enabled;
	int eventsEqualAndNotEmpty = !!(fdSource->pollFd.events & fdSource->pollFd.revents);
	return enabled && eventsEqualAndNotEmpty;
}

/**
 * Check the file descriptor source after poll() was called.
 *
 * @param  source glib source
 * @return        G_SOURCE_CONTINUE if I/O operation is enabled and events are present
 */
static int ioGlibBindingFdCheck (GSource * source)
{
	FdSource * fdSource = (FdSource *) source;
	int enabled = fdSource->bindingData->enabled;
	int eventsEqualAndNotEmpty = !!(fdSource->pollFd.events & fdSource->pollFd.revents);
	return enabled && eventsEqualAndNotEmpty;
}

/**
 * Dispatch the file descriptor source.
 *
 * @param  source   glib source
 * @param  callback source callback (not I/O operation callback). can be set with g_source_set_callback
 * @param  data     source callback data
 * @return          G_SOURCE_CONTINUE or G_SOURCE_REMOVE
 */
static int ioGlibBindingFdDispatch (GSource * source, GSourceFunc callback, void * data)
{
	FdSource * fdSource = (FdSource *) source;
	int flags = eventsToFlags (fdSource->pollFd.revents);

	// clear flag if source is writable
	if ((fdSource->pollFd.revents & G_IO_OUT))
	{
		fdSource->pollFd.revents &= ~G_IO_OUT;
	}

	// clear flag if source is readable
	if ((fdSource->pollFd.revents & G_IO_IN))
	{
		fdSource->pollFd.revents &= ~G_IO_IN;
	}

	if (callback)
	{
		return callback (data);
	}

	// Call operation callback
	GlibBindingData * bindingData = fdSource->bindingData;
	ElektraIoFdOperation * fdOp = (ElektraIoFdOperation *) bindingData->operation.fd;
	elektraIoFdGetCallback (fdOp) (fdOp, flags);

	return G_SOURCE_CONTINUE;
}

/**
 * Cleanup after file descriptor has been detached
 * @param  source glib source
 */
static void ioGlibBindingFdCleanup (GSource * source)
{
	FdSource * fdSource = (FdSource *) source;
	elektraFree (fdSource->bindingData->fdFuncs);
	elektraFree (fdSource->bindingData);
}

/**
 * Calls the associated callback.
 * Called by glib whenever a timer has elapsed.
 *
 * @param data source callback data
 */
static int ioGlibBindingTimerCallback (void * data)
{
	ELEKTRA_NOT_NULL (data);
	GlibBindingData * bindingData = (GlibBindingData *) data;
	ElektraIoTimerOperation * timerOp = (ElektraIoTimerOperation *) bindingData->operation.timer;

	if (bindingData->enabled)
	{
		elektraIoTimerGetCallback (timerOp) (timerOp);
	}

	return G_SOURCE_CONTINUE;
}

/**
 * Calls the associated callback.
 * Called by glib whenever an idle operation can perform its operations.
 *
 * @param data  source callback data
 */
static int ioGlibBindingIdleCallback (void * data)
{
	ELEKTRA_NOT_NULL (data);
	GlibBindingData * bindingData = (GlibBindingData *) data;
	ElektraIoIdleOperation * idleOp = (ElektraIoIdleOperation *) bindingData->operation.idle;

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
	ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	GlibBindingData * bindingData = (GlibBindingData *) elektraIoFdGetBindingData (fdOp);
	bindingData->enabled = elektraIoFdIsEnabled (fdOp);

	FdSource * fdSource = bindingData->source.fdSource;
	GSource * gSource = &bindingData->source.fdSource->gSource;

	if (bindingData->enabled)
	{
		// Update polling flags
		fdSource->pollFd.events = flagsToEvents (elektraIoFdGetFlags (fdOp));

		if (!fdSource->isPolling)
		{
			// Start polling file descriptor
			fdSource->pollFd.fd = elektraIoFdGetFd (fdOp);
			fdSource->pollFd.revents = 0; // clear return events
			g_source_add_poll (gSource, &fdSource->pollFd);
		}
	}

	if (!bindingData->enabled && fdSource->isPolling)
	{
		// Stop polling file descriptor
		g_source_remove_poll (gSource, &fdSource->pollFd);
		fdSource->isPolling = 0;
	}


	return 1;
}

/**
 * Add file descriptor to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddFd
 */
static int ioGlibBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp)
{
	GlibBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	GMainContext * context = (GMainContext *) elektraIoBindingGetData (binding);

	elektraIoFdSetBindingData (fdOp, bindingData);
	bindingData->operation.fd = fdOp;

	// Populate functions for glib source
	GSourceFuncs * funcs = elektraMalloc (sizeof *funcs);
	if (!funcs)
	{
		ELEKTRA_LOG_WARNING ("not enough memory");
		return 0;
	}
	funcs->prepare = ioGlibBindingFdPrepare;
	funcs->check = ioGlibBindingFdCheck;
	funcs->dispatch = ioGlibBindingFdDispatch;
	funcs->finalize = ioGlibBindingFdCleanup;
	bindingData->fdFuncs = funcs;
	FdSource * fdSource = (FdSource *) g_source_new (funcs, sizeof *fdSource);
	GSource * gSource = &fdSource->gSource;

	// Start polling the file descriptor
	fdSource->pollFd.fd = elektraIoFdGetFd (fdOp);
	fdSource->pollFd.events = flagsToEvents (elektraIoFdGetFlags (fdOp));
	fdSource->pollFd.revents = 0; // clear return events
	g_source_add_poll (gSource, &fdSource->pollFd);

	g_source_attach (gSource, context);

	bindingData->source.fdSource = fdSource;
	fdSource->bindingData = bindingData;

	ioGlibBindingUpdateFd (fdOp);

	return 1;
}

/**
 * Remove file descriptor from I/O binding.
 * @see kdbio.h ::ElektraIoBindingRemoveFd
 */
static int ioGlibBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	GlibBindingData * bindingData = (GlibBindingData *) elektraIoFdGetBindingData (fdOp);
	FdSource * fdSource = bindingData->source.fdSource;
	GSource * gSource = &bindingData->source.fdSource->gSource;

	if (fdSource->isPolling)
	{
		g_source_remove_poll (gSource, &fdSource->pollFd);
	}
	g_source_destroy (gSource);
	g_source_unref (gSource);

	return 1;
}

/**
 * Update timer in I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateTimer
 */
static int ioGlibBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	GlibBindingData * bindingData = (GlibBindingData *) elektraIoTimerGetBindingData (timerOp);
	bindingData->enabled = elektraIoTimerIsEnabled (timerOp);

	if (bindingData->cache.timerInterval != elektraIoTimerGetInterval (timerOp))
	{
		// Delete old source
		if (bindingData->source.gSource)
		{
			g_source_destroy (bindingData->source.gSource);
			g_source_unref (bindingData->source.gSource);
		}
		ElektraIoInterface * binding = elektraIoTimerGetBinding (timerOp);
		GMainContext * context = (GMainContext *) elektraIoBindingGetData (binding);

		// (Re-)create timeout source since interval of existing source cannot be changed
		GSource * timeoutSource = g_timeout_source_new (elektraIoTimerGetInterval (timerOp));
		g_source_set_callback (timeoutSource, ioGlibBindingTimerCallback, bindingData, NULL);
		g_source_attach (timeoutSource, context);
		bindingData->source.gSource = timeoutSource;

		bindingData->cache.timerInterval = elektraIoTimerGetInterval (timerOp);
	}

	return 1;
}

/**
 * Add timer for I/O binding.
 * @see kdbio.h ::ElektraIoBindingAddTimer
 */
static int ioGlibBindingAddTimer (ElektraIoInterface * binding ELEKTRA_UNUSED, ElektraIoTimerOperation * timerOp)
{
	GlibBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}

	elektraIoTimerSetBindingData (timerOp, bindingData);
	bindingData->operation.timer = timerOp;

	// Create source
	ioGlibBindingUpdateTimer (timerOp);

	return 1;
}

/**
 * Remove timer from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveTimer
 */
static int ioGlibBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	GlibBindingData * bindingData = (GlibBindingData *) elektraIoTimerGetBindingData (timerOp);

	g_source_destroy (bindingData->source.gSource);
	g_source_unref (bindingData->source.gSource);
	elektraFree (bindingData);

	return 1;
}

/**
 * Update idle operation in I/O binding
 * @see kdbio.h ::ElektraIoBindingUpdateIdle
 */
static int ioGlibBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	ELEKTRA_NOT_NULL (elektraIoIdleGetBindingData (idleOp));
	GlibBindingData * bindingData = (GlibBindingData *) elektraIoIdleGetBindingData (idleOp);
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
	GMainContext * context = (GMainContext *) elektraIoBindingGetData (binding);

	elektraIoIdleSetBindingData (idleOp, bindingData);
	bindingData->operation.idle = idleOp;

	GSource * idleSource = g_idle_source_new ();
	g_source_set_callback (idleSource, ioGlibBindingIdleCallback, bindingData, NULL);
	g_source_attach (idleSource, context);

	bindingData->source.gSource = idleSource;

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
	GlibBindingData * bindingData = (GlibBindingData *) elektraIoIdleGetBindingData (idleOp);

	g_source_destroy (bindingData->source.gSource);
	g_source_unref (bindingData->source.gSource);
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
