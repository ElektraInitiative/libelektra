/**
 * @file
 *
 * @brief Private Elektra-IO structures for I/O bindings, plugins and applications
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IO_PRIVATE_H_
#define KDB_IO_PRIVATE_H_

#include <elektra/kdbio.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * Information about a file descriptor watched by I/O binding
 */
typedef struct _ElektraIoFdOperation
{

	/** Private data */
	void * privateData;

	/** Private data for I/O binding */
	void * bindingData;

	/** Binding instance */
	ElektraIoInterface * binding;

	/** Whether file descriptor is watched for changes, 0 means disabled */
	int enabled;

	/** Called when file descriptor state has changed */
	ElektraIoFdCallback callback;

	/** File descriptor */
	int fd;

	/** Select on which file descriptor status changes the callback should be invoked */
	int flags;
} _ElektraIoFdOperation;

/**
 * Information about a timer in I/O binding
 */
typedef struct _ElektraIoTimerOperation
{

	/** Private data */
	void * privateData;

	/** Private data for I/O binding */
	void * bindingData;

	/** Binding instance */
	ElektraIoInterface * binding;

	/** Whether timer is enabled, 0 means disabled */
	int enabled;

	/** Called when interval has elapsed */
	ElektraIoTimerCallback callback;

	/** Timeout interval in milliseconds (value > 0) */
	unsigned int interval;

} _ElektraIoTimerOperation;

/**
 * Information about an idle task in I/O binding
 */
typedef struct _ElektraIoIdleOperation
{

	/** Private data */
	void * privateData;

	/** Private data for I/O binding */
	void * bindingData;

	/** Binding instance */
	ElektraIoInterface * binding;

	/** Whether timer is enabled, 0 means disabled */
	int enabled;

	/** Called when task can be performed */
	ElektraIoIdleCallback callback;

} _ElektraIoIdleOperation;


/**
 * Elektra's IO-Interface contains pointers to functions for asynchronous operation management.
 * The interface is allocated and populated by one of Elektra's I/O bindings or a custom binding.
 *
 * Using a struct allows to use multiple and different bindings at the same time.
 */
typedef struct _ElektraIoInterface
{
	/**
	 * Private data field.
	 *
	 * Can be used by I/O binding implementations
	 */
	void * data;

	/**
	 * Add file descriptor to be watched by I/O binding
	 *
	 * @param  binding  I/O binding
	 * @param  fdOp     File descriptor information
	 * @return          0 on success, any other value on error
	 */
	int (*addFd) (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp);

	/**
	 * Notify I/O binding about changes to file descriptor structure.
	 *
	 * The following fields are allowed to change: enabled, flags
	 *
	 * @param  fdOp  File descriptor information
	 * @return       0 on success, any other value on error
	 */
	int (*updateFd) (ElektraIoFdOperation * fdOp);

	/**
	 * Remove file descriptor from I/O binding.
	 *
	 * @param  fdOp  File descriptor information
	 * @return       0 on success, any other value on error
	 */
	int (*removeFd) (ElektraIoFdOperation * fdOp);

	/**
	 * Add timer to I/O binding.
	 *
	 * Timeouts callbacks are executed after the initial interval has elapsed and then repeatedly after the interval has elapsed.
	 *
	 * @param  binding     I/O binding
	 * @param  timerOp Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*addTimer) (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp);

	/**
	 * Notifiy I/O binding about changes to timer structure.
	 *
	 * The following fields are allowed to change: enabled, interval
	 *
	 * @param  timerOp Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*updateTimer) (ElektraIoTimerOperation * timerOp);

	/**
	 * Remove timer from I/O binding.
	 *
	 * @param  timerOp Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*removeTimer) (ElektraIoTimerOperation * timerOp);

	/**
	 * Add idle to I/O binding.
	 *
	 * Idle callbacks are executed without negative effects on other IO sources or the application (e.g. next event loop iteration)
	 *
	 * @param  binding     I/O binding
	 * @param  idleOp    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*addIdle) (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp);

	/**
	 * Notifiy I/O binding about changes to idle structure.
	 *
	 * The following fields are allowed to change: enabled
	 *
	 * @param  idleOp    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*updateIdle) (ElektraIoIdleOperation * idleOp);

	/**
	 * Remove idle from I/O binding.
	 *
	 * @param  idleOp    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*removeIdle) (ElektraIoIdleOperation * idleOp);

	/**
	 * Free memory used by I/O binding.
	 *
	 * All added operations have to be removed before calling this function.
	 *
	 * @param  binding     I/O binding
	 * @return             0 on success, any other value on error
	 */
	int (*cleanup) (ElektraIoInterface * binding);

} _ElektraIoInterface;

#ifdef __cplusplus
}
}
#endif

#endif
