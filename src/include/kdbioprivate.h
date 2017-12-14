/**
 * @file
 *
 * @brief Private Elektra-IO structures for IO bindings, plugins and applications
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IO_PRIVATE_H_
#define KDB_IO_PRIVATE_H_

#include <kdbio.h>

/**
 * Information about a file descriptor watched by IO-Binding
 */
typedef struct _ElektraIoFdOperation
{

	/** Private data */
	void * privateData;

	/** Private data for IO binding */
	void * bindingData;

	/** Binding instance */
	ElektraIoInterface * binding;

	/** File descriptor */
	int fd;

	/** Called when file descriptor state has changed */
	ElektraIoFdCallback callback;

	/** Whether file descriptor is watched for changes, 0 means disabled */
	int enabled;

	/** Select on which file descriptor status changes the callback should be invoked */
	int flags;
} _ElektraIoFdOperation;

/**
 * Information about a timer in IO-Binding
 */
typedef struct _ElektraIoTimerOperation
{

	/** Private data */
	void * privateData;

	/** Private data for IO binding */
	void * bindingData;

	/** Binding instance */
	ElektraIoInterface * binding;

	/** Called when interval has elapsed */
	ElektraIoTimerCallback callback;

	/** Whether timer is enabled, 0 means disabled */
	int enabled;

	/**
	 * Timeout interval in milliseconds (value > 0)
	 */
	unsigned int interval;

} _ElektraIoTimerOperation;

/**
 * Information about an idle task in IO-Binding
 */
typedef struct _ElektraIoIdleOperation
{

	/** Private data */
	void * privateData;

	/** Private data for IO binding */
	void * bindingData;

	/** Binding instance */
	ElektraIoInterface * binding;

	/** Called when task can be performed */
	ElektraIoIdleCallback callback;

	/** Whether timer is enabled, 0 means disabled */
	int enabled;

} _ElektraIoIdleOperation;


/**
 * Elektra's IO-Interface contains pointers to functions for asynchronous operation management.
 * The interface is allocated and populated by one of Elektra's IO-Bindings or a custom binding.
 *
 * Using a struct allows to use multiple and different bindings at the same time.
 */
typedef struct _ElektraIoInterface
{
	/**
	 * Private data field.
	 * Can be used by IO-Binding implementations
	 */
	void * data;

	/**
	 * Add file descriptor to be watched by IO-Binding
	 *
	 * @param  binding  IO-Binding
	 * @param  fdOp     File descriptor information
	 * @return          0 on success, any other value on error
	 */
	int (*addFd) (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp);

	/**
	 * Notify IO-Binding about changes to file descriptor structure.
	 * The following fields are allowed to change: enabled, events
	 *
	 * @param  fdOp  File descriptor information
	 * @return       0 on success, any other value on error
	 */
	int (*updateFd) (ElektraIoFdOperation * fdOp);

	/**
	 * Remove file descriptor from IO-Binding
	 *
	 * @param  fdOp  File descriptor information
	 * @return       0 on success, any other value on error
	 */
	int (*removeFd) (ElektraIoFdOperation * fdOp);

	/**
	 * Add timer to IO-Binding.
	 * Timeouts callbacks are executed after the initial interval has elapsed and then repeatedly after the interval has elapsed.
	 *
	 * @param  binding     IO-Binding
	 * @param  timerOp Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*addTimer) (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp);

	/**
	 * Notifiy IO-Binding about changes to timer structure
	 * The following fields are allowed to change: enabled, interval
	 *
	 * @param  timerOp Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*updateTimer) (ElektraIoTimerOperation * timerOp);

	/**
	 * Remove timer from IO-Binding
	 * @param  timerOp Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*removeTimer) (ElektraIoTimerOperation * timerOp);

	/**
	 * Add idle to IO-Binding.
	 * Idle callbacks are executed without negative effects on other IO sources or the application (e.g. next event loop iteration)
	 *
	 * @param  binding     IO-Binding
	 * @param  idleOp    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*addIdle) (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp);

	/**
	 * Notifiy IO-Binding about changes to idle structure
	 * The following fields are allowed to change: enabled
	 *
	 * @param  idleOp    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*updateIdle) (ElektraIoIdleOperation * idleOp);

	/**
	 * Remove idle from IO-Binding
	 *
	 * @param  idleOp    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*removeIdle) (ElektraIoIdleOperation * idleOp);

	/**
	 * Free memory used by IO-Binding.
	 * All added file descriptors and timers have to be removed before calling this function.
	 *
	 * @param  binding     IO-Binding
	 * @return             0 on success, any other value on error
	 */
	int (*cleanup) (ElektraIoInterface * binding);

} _ElektraIoInterface;

#endif
