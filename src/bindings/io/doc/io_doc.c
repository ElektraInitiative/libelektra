/**
 * @file
 *
 * @brief I/O example binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * All functions except the I/O binding entry point (elektraIoDocNew()) would
 * normally be static but for documentation purposes they are not.
 */


/**
 * @defgroup kdbio I/O Bindings
 *
 * @brief Asynchronous I/O feature
 *
 * @section async Asynchronous I/O with Elektra
 *
 * @subsection Overview
 *
 * I/O bindings allow Elektra and its plugins to integrate into different main
 * loop APIs using a thin abstraction layer.
 * For example, this is used for notification transport plugins which receive
 * notifications using ZeroMQ, D-Bus, etc.
 *
 * I/O bindings are created using an initialization function for a specific main
 * loop API.
 * Please see [bindings](https://www.libelektra.org/bindings/readme)
 * for available I/O bindings and their according READMEs for more details.
 * After creating, an I/O binding is associated to a KDB instance using
 * elektraIoSetBinding().
 * Having different I/O bindings (e.g. same or different main loop APIs) for
 * different KDB instances is supported.
 *
 * The remainder of this page contains useful details for creating I/O bindings
 * and using the operations provided by these bindings.
 * Application developers are normally not required to do those
 * tasks.
 * For more information about using I/O bindings from an application developer
 * perspective please read the
 * <a href="doc_tutorials_notifications_md.html">Notification Tutorial</a>.
 *
 * @subsection Introduction
 *
 * An I/O binding needs to handle different types of operations.
 * These operations are used by plugins that require asynchronous I/O.
 * In this document we will call developers of these plugins "users".
 * The three types of operations are:
 * - file descriptor watch operations
 * - timer operations
 * - idle operations
 *
 * Each operation has a user callback that is called under the following
 * conditions:
 * - For file descriptor watch operations a user callback shall be called
 *   whenever the descriptor status matches the "flags" bitmask (readable,
 *   writable or both).
 * - For timer operations a user callback shall be called whenever a given
 *   interval has elapsed.
 * - Idle operations shall call a user callback whenever possible without
 *   interfering other operations.
 *   These operations are used for bulk data transfers or expensive
 *   calculations that would stall other operations when done at once.
 *
 * Each operation has different properties. The following properties are
 * shared by all operations:
 * - callback (elektraIoFdGetCallback();set by elektraIoNewFdOperation())
 *   user callback
 * - enabled (elektraIoFdIsEnabled();elektraIoFdSetEnabled()) indicates whether
 *   the operation is enabled. No callback shall be fired when an operation is
 *   disabled.
 * - privateData (elektraIoFdGetData();set by elektraIoNewFdOperation())
 *   pointer to data owned by user
 * - bindingData (elektraIoFdGetBindingData();elektraIoFdSetBindingData())
 *   pointer to data owned by I/O binding
 *
 * For brevity only file descriptor operation variants are listed here.
 * Variants for timer and idle operations are called `elektraIoTimer*` and
 * `elektraIoIdle*`.
 * All `elektraIo*` utility functions are provided by the `elektra-io` library.
 *
 * File descriptor watch operations have the following additional properties:
 * - fd (elektraIoFdGetFd();set by elektraIoNewFdOperation()) file descriptor
 * number to be watched
 * - flags (elektraIoFdGetFlags();elektraIoFdSetFlags()) flags bitmask
 *
 * Timer operations have the following additional properties:
 * - interval (elektraIoTimerGetInterval();elektraIoTimerSetInterval()) minimum
 *   interval in milliseconds after the user callback shall be fired
 *
 * Idle operations have no additional properties.
 *
 * @par Creating a new I/O Binding
 *
 * Every I/O binding needs to provide ten functions:
 * - file descriptor watch operations
 *    - ::ElektraIoBindingAddFd (e.g. ioDocBindingAddFd())
 *    - ::ElektraIoBindingUpdateFd (e.g. ioDocBindingUpdateFd())
 *    - ::ElektraIoBindingRemoveFd (e.g. ioDocBindingRemoveFd())
 * - timer operations
 *    - ::ElektraIoBindingAddTimer (e.g. ioDocBindingAddTimer())
 *    - ::ElektraIoBindingUpdateTimer (e.g. ioDocBindingUpdateTimer())
 *    - ::ElektraIoBindingRemoveTimer (e.g. ioDocBindingRemoveTimer())
 * - idle operations
 *    - ::ElektraIoBindingAddIdle (e.g. ioDocBindingAddIdle())
 *    - ::ElektraIoBindingUpdateIdle (e.g ioDocBindingUpdateIdle())
 *    - ::ElektraIoBindingRemoveIdle (e.g. ioDocBindingRemoveIdle())
 * - ::ElektraIoBindingCleanup (e.g. ioDocBindingCleanup())
 *
 * In order to create a new I/O binding you have to create an entry point for your
 * binding (e.g. elektraIoDocNew()). This entry point then calls
 * elektraIoNewBinding() with pointers to the ten required functions.
 *
 * @snippet this kdbio binding create
 *
 * If your I/O management library requires you to store additional data you can
 * do so using elektraIoBindingSetData().
 * Let's assume you have the following data structure:
 *
 * @snippet this kdbio binding data
 *
 * Then you can store your data with the I/O binding.
 *
 * @snippet this kdbio binding setdata
 *
 * Of course if you need to store only a single pointer (e.g. a handle) you can
 * omit the struct and directly use elektraIoBindingSetData() with your pointer.
 *
 * @par Implementing Operations
 *
 * The next step is to implement operation functions.
 * We'll walk through the implementation of the functions for managing
 * file descriptor watch operations.
 * Timer and idle variants are the same except for the operation properties.
 *
 * For reconstructing the user callback it is advisable to store a context for
 * each operation in your I/O management library.
 * Most I/O management libraries let you pass this context when adding an
 * operation to the library.
 * This context is then passed by the library back to your callbacks.
 * You can use the operation data itself as context and store additional data
 * like handles from your I/O management library by using
 * elektraIoFdSetBindingData().
 *
 * Let's assume the data structure looks like this:
 *
 * @snippet this kdbio operation data
 *
 * Using this struct's members you can store additional data like handles in
 * operations.
 * The member `bar` is just an example.
 *
 * The following snippet from ioDocBindingAddFd() shows example code for
 * ::ElektraIoBindingAddFd.
 * Code for ::ElektraIoBindingAddTimer and ::ElektraIoBindingAddIdle is similar.
 *
 * @snippet this kdbio binding addfd
 *
 * In ::ElektraIoBindingUpdateFd or ::ElektraIoBindingRemoveFd you can access
 * your binding operation data by using elektraIoFdGetBindingData().
 *
 * @snippet this kdbio operation getdata
 *
 * When your I/O management library detects a change of the file descriptor
 * status it will call a callback supplied by your I/O binding.
 * We will assume for file descriptor watch operations this is
 * ioDocBindingFdCallback().
 * Your I/O binding's task is to call the operation callback supplied by the
 * user with the correct arguments.
 *
 * @snippet this kdbio operation callback
 *
 * We assumed `SomeIoLibHandle->data` let's you access your context.
 * Since we have used the original operation data as context we directly obtain
 * the operation data to retrieve the user callback using
 * elektraIoFdGetCallback().
 * Additionally it is necessary to convert the I/O management library's bitmask
 * to Elekta's I/O bitmask (::ElektraIoFdFlags) and then call the user callback.
 *
 * When implementing ::ElektraIoBindingRemoveFd (or the timer and idle
 * equivalents) make sure to free data allocated in the add functions.
 *
 * @par Cleanup
 *
 * ::ElektraIoBindingCleanup is the place to free data allocated for your I/O
 * binding.
 *
 * At least you need to free the pointer returned from
 * elektraIoNewBinding() in your I/O binding's entry point.
 *
 * @par Linking
 *
 * Make sure to link against the `elektra-io` library for the `elektraIo*`
 * utility functions that create bindings or operations and allow access to
 * their fields.
 * This library is available via `pkg-config`.
 *
 * @par Testing
 *
 * Elektra provides a test suite for I/O bindings in order to make sure that
 * transport plugins will work with all bindings.
 * To run the test suite you need to execute elektraIoTestSuite() and provide
 * the necessary callbacks for creating a new binding, starting and stopping
 * asynchronous processing (::ElektraIoTestSuiteCreateBinding,
 * ::ElektraIoTestSuiteStart and ::ElektraIoTestSuiteStop).
 *
 * @snippet testio_doc.c kdbio testsuite main
 *
 * The functions supplied to elektraIoTestSuite() are called for setup, starting
 * and stopping of the tests.
 *
 * For example ::ElektraIoTestSuiteCreateBinding of the "doc" binding:
 *
 * @snippet testio_doc.c kdbio testsuite create
 *
 * Of course starting and stopping is specific to your I/O management library.
 */
#include <stdlib.h>
#include <string.h>

#include <elektra/io/api.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>
#include <internal/utility/alloc.h>
/**
 * Example I/O management library data structure
 */
typedef struct SomeIoLibHandle
{
	/** Let's you access the context you supplied to the I/O management library */
	void * data;
} SomeIoLibHandle;

/**
 * Example I/O mangement library bitmask flags
 */
typedef enum
{

	/** indicates that the file descriptor is readable */
	SOME_IOLIB_READABLE = 1 << 0,

	/** indicates that the file descriptor is readable */
	SOME_IOLIB_WRITABLE = 1 << 1,

} SomeIoLibFlags;

//! [kdbio operation data]
/**
 * Container for additional information for I/O binding operations.
 *
 * It is helpful to create a data structure for your binding to store additional data
 */
typedef struct DocOperationData
{
	/** Example member */
	char * bar;
	// Add additional members as required
} DocOperationData;
//! [kdbio operation data]

//! [kdbio binding data]
/**
 * Container for additional information for an I/O binding.
 */
typedef struct DocBindingData
{
	/** Example member */
	char * foo;
	// Add additional members as required
} DocBindingData;
//! [kdbio binding data]

/**
 * A little helper function for allocating our custom operation data structure
 * @return  New data structure, remember to call elektraFree()
 */
/*static*/ DocOperationData * newOperationData (void)
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
 * Convert your I/O library bit mask to Elektra's I/O flags
 * @param  bitmask   bit mask from I/O management library
 * @return           bit mask with Elektra's I/O flags (::ElektraIoFdFlags)
 */
/*static*/ int someBitMaskToElektraIoFlags (int bitmask)
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
 * Calls the associated operation callback.
 * Called by your I/O management library whenever a file descriptor status has changed.
 *
 * @param handle some I/O management handle
 * @param flags flags bit mask
 */
//! [kdbio operation callback]
/*static*/ void ioDocBindingFdCallback (SomeIoLibHandle * handle, int bitmask)
{
	// For this example let's assume handle is passed as argument
	ELEKTRA_NOT_NULL (handle->data);
	ElektraIoFdOperation * fdOp = (ElektraIoFdOperation *) handle->data;

	// Convert bitmask to Elekta's flags
	elektraIoFdGetCallback (fdOp) (fdOp, someBitMaskToElektraIoFlags (bitmask));
}
//! [kdbio operation callback]

/**
 * Calls the associated operation callback.
 * Called by your I/O management library whenever a timer interval has passed.
 *
 * @param handle some I/O management handle
 */
/*static*/ void ioDocBindingTimerCallback (SomeIoLibHandle * handle)
{
	// For this example let's assume handle was passed as argument
	ELEKTRA_NOT_NULL (handle->data);
	ElektraIoTimerOperation * timerOp = (ElektraIoTimerOperation *) handle->data;
	DocOperationData * operationData = (DocOperationData *) elektraIoTimerGetBindingData (timerOp);

	elektraIoTimerGetCallback (timerOp) (timerOp);
}

/**
 * Calls the associated operation callback.
 * Called by your I/O management library whenever an idle operation can run
 * without interfering with other operations.
 *
 * @param handle some I/O management handle
 */
/*static*/ void ioDocBindingIdleCallback (SomeIoLibHandle * handle)
{
	// For this example let's assume handle was passed as argument
	ELEKTRA_NOT_NULL (handle->data);
	ElektraIoIdleOperation * idleOp = (ElektraIoIdleOperation *) handle->data;
	DocOperationData * operationData = (DocOperationData *) elektraIoIdleGetBindingData (idleOp);

	elektraIoIdleGetCallback (idleOp) (idleOp);
}

/**
 * Update information about a file descriptor watched by I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateFd
 */
/*static*/ int ioDocBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	DocOperationData * operationData = (DocOperationData *) elektraIoFdGetBindingData (fdOp);

	// Based on elektraIoFdIsEnabled() enable or disable the operation
	// Based on elektraIoFdGetFlags() set the flag bitmask (will need conversion)
	// someIoLibUpdateFd (operationData->handle, elektraIoFdGetFd (fdOp), elektraIoFdGetFlags (fdOp));

	return 1;
}

/**
 * Add file descriptor to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddFd
 */
/*static*/ int ioDocBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp)
{
	//! [kdbio binding addfd]
	DocOperationData * operationData = newOperationData ();
	if (operationData == NULL)
	{
		return 0;
	}
	// You can use private data stored in the I/O binding
	// e.g. MyData data = (MyData *)elektraIoBindingGetData (binding);

	elektraIoFdSetBindingData (fdOp, operationData);
	// You can store additional data for each operation in your operationData structure
	operationData->bar = "foo";

	// Here you need to add the operation to the I/O management library
	// ioDocBindingFdCallback() holds an example callback to pass to your I/O management library

	// assume SomeIoLibHandle * someIoLibAddFd (int fd, int flags, int enabled, void * privateData, callback)
	// operationData->handle = someIoLibAddFd (elektraIoFdGetFd (fdOp), elektraIoFdGetFlags (fdOp), elektraIoFdIsEnabled (fdOp), &fdOp,
	// ioDocBindingFdCallback)

	return 1;
	//! [kdbio binding addfd]
}

/**
 * Remove file descriptor from I/O binding.
 * @see kdbio.h ::ElektraIoBindingRemoveFd
 */
/*static*/ int ioDocBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	//! [kdbio operation getdata]
	DocOperationData * operationData = (DocOperationData *) elektraIoFdGetBindingData (fdOp);
	//! [kdbio operation getdata]

	// Stop polling of the file descriptor and free data afterwards
	// someIoLibStopFd (operationData->handle);
	elektraFree (operationData);

	return 1;
}

/**
 * Update timer in I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateTimer
 */
/*static*/ int ioDocBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	DocOperationData * operationData = (DocOperationData *) elektraIoTimerGetBindingData (timerOp);

	// Based on elektraIoTimerIsEnabled (timerOp) enable or disable the operation
	// Based on elektraIoTimerGetInterval (timerOp) change the timer interval
	// someIoLibUpdateTimer (operationData->handle, elektraIoTimerGetInterval (timerOp), elektraIoTimerIsEnabled (timerOp));

	return 1;
}

/**
 * Add timer for I/O binding.
 * @see kdbio.h ::ElektraIoBindingAddTimer
 */
/*static*/ int ioDocBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp)
{
	DocOperationData * operationData = newOperationData ();
	if (operationData == NULL)
	{
		return 0;
	}
	// You can use private data stored in the I/O binding
	// e.g. MyData data = (MyData *)elektraIoBindingGetData (binding);

	elektraIoTimerSetBindingData (timerOp, operationData);
	// You can store additional data for each operation in your operationData structure
	operationData->bar = "foo";

	// Here you need to add the operation to the I/O management library
	// ioDocBindingTimerCallback holds an example callback to pass to your I/O management library

	// assume SomeIoLibHandle * someIoLibAddTimer(int interval, int enabled, void * privateData, callback)
	// operationData->handle = someIoLibAddTimer(elektraIoTimerGetInterval (timerOp), elektraIoTimerIsEnabled (timerOp), &timerOp,
	// ioDocBindingTimerCallback)

	return 1;
}

/**
 * Remove timer from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveTimer
 */
/*static*/ int ioDocBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	DocOperationData * operationData = (DocOperationData *) elektraIoTimerGetBindingData (timerOp);

	// Stop polling of the file descriptor and free data afterwards
	// someIoLibStopTimer (operationData->handle);
	elektraFree (operationData);

	return 1;
}

/**
 * Update idle operation in I/O binding
 * @see kdbio.h ::ElektraIoBindingUpdateIdle
 */
/*static*/ int ioDocBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	ELEKTRA_NOT_NULL (elektraIoIdleGetBindingData (idleOp));
	DocOperationData * operationData = (DocOperationData *) elektraIoIdleGetBindingData (idleOp);

	// Based on elektraIoIdleIsEnabled (idleOp) enable or disable the operation
	// someIoLibUpdateIdle (operationData->handle, elektraIoIdleIsEnabled (idleOp));

	return 1;
}

/**
 * Add idle operation to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddIdle
 */
/*static*/ int ioDocBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp)
{
	DocOperationData * operationData = newOperationData ();
	if (operationData == NULL)
	{
		return 0;
	}
	// You can use private data stored in the I/O binding
	// e.g. MyData data = (MyData *)elektraIoBindingGetData (binding);

	elektraIoIdleSetBindingData (idleOp, operationData);
	// You can store additional data for each operation in your operationData structure
	operationData->bar = "foo";

	// Here you need to add the operation to the I/O management library
	// ioDocBindingIdleCallback holds an example callback to pass to your I/O management library

	// assume SomeIoLibHandle * someIoLibAddIdle(int enabled, void * privateData, callback)
	// operationData->handle = someIoLibAddIdle(elektraIoIdleIsEnabled (idleOp), &idleOp, ioDocBindingIdleCallback)

	return 1;
}

/**
 * Remove idle operation from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveIdle
 */
/*static*/ int ioDocBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	ELEKTRA_NOT_NULL (elektraIoIdleGetBindingData (idleOp));
	DocOperationData * operationData = (DocOperationData *) elektraIoIdleGetBindingData (idleOp);

	// Stop polling of the file descriptor and free data afterwards
	// someIoLibStopIdle (operationData->handle);
	elektraFree (operationData);

	return 1;
}

/**
 * Cleanup
 * @param  binding I/O binding
 * @see kdbio.h ::ElektraIoBindingCleanup
 */
/*static*/ int ioDocBindingCleanup (ElektraIoInterface * binding)
{
	ELEKTRA_NOT_NULL (binding);
	elektraFree (elektraIoBindingGetData (binding));
	elektraFree (binding);
	return 1;
}

/**
 * Create and initialize a new doc I/O binding
 * @param  foo Some data from I/O management library (e.g. a handle)
 * @return     Populated I/O interface
 */
ElektraIoInterface * elektraIoDocNew (char * foo)
{
	//! [kdbio binding create]
	// Initialize I/O interface
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
	//! [kdbio binding create]
	//! [kdbio binding setdata]
	// Store binding relevant data in the interface
	DocBindingData * bindingData = elektraMalloc (sizeof (*bindingData));
	if (bindingData == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraMalloc failed");
		return NULL;
	}
	elektraIoBindingSetData (binding, bindingData);
	bindingData->foo = foo;
	//! [kdbio binding setdata]

	return binding;
}
