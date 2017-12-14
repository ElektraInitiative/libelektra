/**
 * @file
 *
 * @brief IO Example binding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbio.h>
#include <kdblogger.h>

// Example I/O management library data structure
typedef struct SomeIoLibHandle
{
	void * data;
} SomeIoLibHandle;

// Example I/O mangement library bitmask flags
typedef enum {
	SOME_IOLIB_READABLE = 1 << 0,
	SOME_IOLIB_WRITABLE = 1 << 1,
} SomeIoLibFlags;

/**
 * Container for required additional information for
 * IO binding operations
 *
 * It is helpful to create a data structure for your binding to store additional data
 */
typedef struct DocOperationData
{
	union {
		ElektraIoFdOperation * fd;
		ElektraIoTimerOperation * timer;
		ElektraIoIdleOperation * idle;
	} operation;
	// Add additional fields as required
	char * bar;
} DocOperationData;

typedef struct DocBindingData
{
	// Add additional fields as required
	char * foo;
} DocBindingData;

/**
 * A little helper function for allocating our custom operation data structure
 * @return  New data structure, remember to call elektraFree()
 */
static DocOperationData * newOperationData (void)
{
	DocOperationData * operationData = elektraMalloc (sizeof (*operationData));
	if (operationData == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}
	memset (operationData, 0, sizeof (*operationData));

	return operationData;
}

/**
 * Convert your I/O library bit mask to Elektra's IO flags
 * @param  bitmask   bit mask from I/O management library
 * @return           bit mask with Elektra's IO flags
 */
static int someBitMaskToElektraIoFlags (int bitmask)
{
	int flags = 0;
	if (bitmask & SOME_IOLIB_READABLE)
	{
		flags |= ELEKTRA_IO_READABLE;
	}
	if (bitmask & SOME_IOLIB_READABLE)
	{
		flags |= ELEKTRA_IO_WRITABLE;
	}
	return flags;
}

/**
 * Calls the associated operation callback
 * Called by your I/O management library whenever a file descriptor status has changed.
 *
 * @param handle some I/O management handle
 * @param flags flags bit mask (already in Elektra's format)
 */
static void ioDocBindingFdCallback (SomeIoLibHandle * handle, int bitmask)
{
	// For this example let's assume handle was passed as argument
	assert (handle->data != NULL);
	DocOperationData * operationData = (DocOperationData *)handle->data;
	ElektraIoFdOperation * fdOp = (ElektraIoFdOperation *)operationData->operation.fd;

	// Flag bit mask will not match ELEKTRA_IO_WRITABLE or ELEKTRA_IO_READABLE values
	elektraIoFdGetCallback (fdOp) (fdOp, someBitMaskToElektraIoFlags (bitmask));
}

/**
 * Calls the associated operation callback
 * Called by your I/O management library whenever a timer interval has passed.
 *
 * @param handle some I/O management handle
 */
static void ioDocBindingTimerCallback (SomeIoLibHandle * handle)
{
	// For this example let's assume handle was passed as argument
	assert (handle->data != NULL);
	DocOperationData * operationData = (DocOperationData *)handle->data;
	ElektraIoTimerOperation * timerOp = (ElektraIoTimerOperation *)operationData->operation.timer;

	elektraIoTimerGetCallback (timerOp) (timerOp);
}

/**
 * Calls the associated operation callback
 * Called by your I/O management library whenever a timer interval has passed.
 *
 * @param handle some I/O management handle
 */
static void ioDocBindingIdleCallback (SomeIoLibHandle * handle)
{
	// For this example let's assume handle was passed as argument
	assert (handle->data != NULL);
	DocOperationData * operationData = (DocOperationData *)handle->data;
	ElektraIoIdleOperation * idleOp = (ElektraIoIdleOperation *)operationData->operation.idle;

	elektraIoIdleGetCallback (idleOp) (idleOp);
}

/**
 * Update information about a file descriptor watched by IO-Binding.
 * @see kdbio.h ElektraIoInterface::updateFd
 */
static int ioDocBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	assert (elektraIoFdGetBindingData (fdOp) != NULL);
	DocOperationData * operationData = (DocOperationData *)elektraIoFdGetBindingData (fdOp);

	// Based on elektraIoFdIsEnabled() enable or disable the operation
	// Based on elektraIoFdGetFlags() set the flag bitmask (will need conversion)
	// someIoLibUpdateFd (operationData->handle, elektraIoFdGetFd (fdOp), elektraIoFdGetFlags (fdOp));

	return 0;
}

/**
 * Add file descriptor to IO-Binding
 * @see kdbio.h ElektraIoInterface::addFd
 */
static int ioDocBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp)
{
	DocOperationData * operationData = newOperationData ();
	if (operationData == NULL)
	{
		return -1;
	}
	// You can use private data stored in the IO binding
	// e.g. MyData data = (MyData *)elektraIoBindingGetData (binding);

	elektraIoFdSetBindingData (fdOp, operationData);
	// You can store additional data for each operation in your operationData structure
	operationData->operation.fd = fdOp;
	operationData->bar = "foo";

	// Here you need to add the operation to the I/O management library
	// ioDocBindingFdCallback holds an example callback to pass to your I/O management library

	// assume SomeIoLibHandle * someIoLibAddFd(int fd, int flags, int enabled, void * privateData, callback)
	// operationData->handle = someIoLibAddFd(elektraIoFdGetFd (fdOp), elektraIoFdGetFlags (fdOp), elektraIoFdIsEnabled (fdOp), &fdOp,
	// ioDocBindingFdCallback)

	return 0;
}

/**
 * Remove file descriptor from IO-Binding.
 * @see kdbio.h ElektraIoInterface::removeFd
 */
static int ioDocBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	assert (elektraIoFdGetBindingData (fdOp) != NULL);
	DocOperationData * operationData = (DocOperationData *)elektraIoFdGetBindingData (fdOp);

	// Stop polling of the file descriptor and free data afterwards
	// someIoLibStopFd (operationData->handle);
	elektraFree (operationData);

	return 0;
}

/**
 * Update timer in IO-Binding.
 * @see kdbio.h ElektraIoInterface::removeFd
 */
static int ioDocBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	assert (elektraIoTimerGetBindingData (timerOp) != NULL);
	DocOperationData * operationData = (DocOperationData *)elektraIoTimerGetBindingData (timerOp);

	// Based on elektraIoTimerIsEnabled (timerOp) enable or disable the operation
	// Based on elektraIoTimerGetInterval (timerOp) change the timer interval
	// someIoLibUpdateTimer (operationData->handle, elektraIoTimerGetInterval (timerOp), elektraIoTimerIsEnabled (timerOp));

	return 0;
}

/**
 * Add timer for IO-Binding.
 * @see kdbio.h ElektraIoInterface::addTimer
 */
static int ioDocBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp)
{
	DocOperationData * operationData = newOperationData ();
	if (operationData == NULL)
	{
		return -1;
	}
	// You can use private data stored in the IO binding
	// e.g. MyData data = (MyData *)elektraIoBindingGetData (binding);

	elektraIoTimerSetBindingData (timerOp, operationData);
	// You can store additional data for each operation in your operationData structure
	operationData->operation.timer = timerOp;

	// Here you need to add the operation to the I/O management library
	// ioDocBindingTimerCallback holds an example callback to pass to your I/O management library

	// assume SomeIoLibHandle * someIoLibAddTimer(int interval, int enabled, void * privateData, callback)
	// operationData->handle = someIoLibAddTimer(elektraIoTimerGetInterval (timerOp), elektraIoTimerIsEnabled (timerOp), &timerOp,
	// ioDocBindingTimerCallback)

	return 0;
}

/**
 * Remove timer from IO-Binding
 * @see kdbio.h ElektraIoInterface::removeTimer
 */
static int ioDocBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	assert (elektraIoTimerGetBindingData (timerOp) != NULL);
	DocOperationData * operationData = (DocOperationData *)elektraIoTimerGetBindingData (timerOp);

	// Stop polling of the file descriptor and free data afterwards
	// someIoLibStopTimer (operationData->handle);
	elektraFree (operationData);

	return 0;
}

/**
 * Update idle operation in IO-Binding
 * @see kdbio.h ElektraIoInterface::updateIdle
 */
static int ioDocBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	assert (elektraIoIdleGetBindingData (idleOp) != NULL);
	DocOperationData * operationData = (DocOperationData *)elektraIoIdleGetBindingData (idleOp);

	// Based on elektraIoIdleIsEnabled (idleOp) enable or disable the operation
	// someIoLibUpdateIdle (operationData->handle, elektraIoIdleIsEnabled (idleOp));

	return 0;
}

/**
 * Add idle operation to IO-Binding
 * @see kdbio.h ElektraIoInterface::addIdle
 */
static int ioDocBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp)
{
	DocOperationData * operationData = newOperationData ();
	if (operationData == NULL)
	{
		return -1;
	}
	// You can use private data stored in the IO binding
	// e.g. MyData data = (MyData *)elektraIoBindingGetData (binding);

	elektraIoIdleSetBindingData (idleOp, operationData);
	// You can store additional data for each operation in your operationData structure
	operationData->operation.idle = idleOp;

	// Here you need to add the operation to the I/O management library
	// ioDocBindingIdleCallback holds an example callback to pass to your I/O management library

	// assume SomeIoLibHandle * someIoLibAddIdle(int enabled, void * privateData, callback)
	// operationData->handle = someIoLibAddIdle(elektraIoIdleIsEnabled (idleOp), &idleOp, ioDocBindingIdleCallback)

	return 0;
}

/**
 * Remove idle operation from IO-Binding
 * @see kdbio.h ElektraIoInterface::removeIdle
 */
static int ioDocBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	assert (elektraIoIdleGetBindingData (idleOp) != NULL);
	DocOperationData * operationData = (DocOperationData *)elektraIoIdleGetBindingData (idleOp);

	// Stop polling of the file descriptor and free data afterwards
	// someIoLibStopIdle (operationData->handle);
	elektraFree (operationData);

	return 0;
}

/**
 * Cleanup
 * @param  binding IO-Binding
 * @see kdbio.h ElektraIoInterface::cleanup
 */
static int ioDocBindingCleanup (ElektraIoInterface * binding)
{
	assert (binding != NULL);
	elektraFree (elektraIoBindingGetData (binding));
	elektraFree (binding);
	return 0;
}

/**
 * Create and initialize a new doc IO binding
 * @param  foo Some data from I/O management library (e.g. a handle)
 * @return     Populated I/O interface
 */
ElektraIoInterface * elektraIoDocNew (char * foo)
{
	// Initialize io interface
	ElektraIoInterface * binding = elektraIoNewBinding (
		// file descriptors
		ioDocBindingAddFd, ioDocBindingUpdateFd, ioDocBindingRemoveFd,
		// timers
		ioDocBindingAddTimer, ioDocBindingUpdateTimer, ioDocBindingRemoveTimer,
		// idle
		ioDocBindingAddIdle, ioDocBindingUpdateIdle, ioDocBindingRemoveIdle,
		// cleanup
		ioDocBindingCleanup);
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraIoNewBinding failed");
		return NULL;
	}

	// Store binding relevant data in the interface
	DocBindingData * bindingData = elektraMalloc (sizeof (*bindingData));
	if (bindingData == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}
	elektraIoBindingSetData (binding, bindingData);
	bindingData->foo = foo;

	return binding;
}
