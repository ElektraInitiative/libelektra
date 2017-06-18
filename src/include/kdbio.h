/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IO_H_
#define KDB_IO_H_

struct ElektraIoInterface;

struct ElektraIoFdOperation;

/**
 * Information about a file descriptor watched by IO-Binding
 */
typedef struct ElektraIoFdOperation
{

	/** Private data for library adapter */
	void * adapterData;

	/** Private data for IO binding */
	void * bindingData;

	/** Binding instance */
	struct ElektraIoInterface * binding;

	/** File descriptor */
	int fd;

	/** Called when file descriptor state has changed */
	void (*callback) (struct ElektraIoFdOperation * fdInfo, int flags);

	/** Whether file descriptor is watched for changes, 0 means disabled */
	int enabled;

	/** Select on which file descriptor status changes the callback should be invoked */
	int flags;
} ElektraIoFdOperation;

/**
 * Available flags for file descriptors
 */
typedef enum {
	ELEKTRA_IO_READABLE = 1 << 0,
	ELEKTRA_IO_WRITABLE = 1 << 1,
} ElektraIoFdFlags;

struct ElektraIoTimerOperation;

/**
 * Information about a timer in IO-Binding
 */
typedef struct ElektraIoTimerOperation
{

	/** Private data for library adapter */
	void * adapterData;

	/** Private data for IO binding */
	void * bindingData;

	/** Binding instance */
	struct ElektraIoInterface * binding;

	/** Called when interval has elapsed */
	void (*callback) (struct ElektraIoTimerOperation * timerInfo);

	/** Whether timer is enabled, 0 means disabled */
	int enabled;

	/**
	 * Timeout interval in milliseconds (value > 0)
	 */
	unsigned int interval;

} ElektraIoTimerOperation;

struct ElektraIoIdleOperation;

/**
 * Information about an idle task in IO-Binding
 */
typedef struct ElektraIoIdleOperation
{

	/** Private data for library adapter */
	void * adapterData;

	/** Private data for IO binding */
	void * bindingData;

	/** Binding instance */
	struct ElektraIoInterface * binding;

	/** Called when task can be performed */
	void (*callback) (struct ElektraIoIdleOperation * idleInfo);

	/** Whether timer is enabled, 0 means disabled */
	int enabled;

} ElektraIoIdleOperation;


/**
 * Elektra's IO-Interface contains pointers to functions for asynchronous operation management.
 * The interface is allocated and populated by one of Elektra's IO-Bindings or a custom binding.
 *
 * Using a struct allows to use multiple and different bindings at the same time.
 */
typedef struct ElektraIoInterface
{
	/**
	 * Private data field.
	 * Can be used by IO-Binding implementations
	 */
	void * private;

	/**
	 * Add file descriptor to be watched by IO-Binding
	 *
	 * @param  binding   IO-Binding
	 * @param  fdInfo    File descriptor information
	 * @return           0 on success, any other value on error
	 */
	int (*addFd) (struct ElektraIoInterface * binding, ElektraIoFdOperation * fdInfo);

	/**
	 * Notify IO-Binding about changes to file descriptor structure.
	 * The following fields are allowed to change: enabled, events
	 *
	 * @param  fdInfo  File descriptor information
	 * @return         0 on success, any other value on error
	 */
	int (*updateFd) (ElektraIoFdOperation * fdInfo);

	/**
	 * Remove file descriptor from IO-Binding
	 *
	 * @param  fdInfo  File descriptor information
	 * @return         0 on success, any other value on error
	 */
	int (*removeFd) (ElektraIoFdOperation * fdInfo);

	/**
	 * Add timer to IO-Binding.
	 * Timeouts callbacks are executed after the initial interval has elapsed and then repeatedly after the interval has elapsed.
	 *
	 * @param  binding     IO-Binding
	 * @param  timerInfo Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*addTimer) (struct ElektraIoInterface * binding, ElektraIoTimerOperation * timerInfo);

	/**
	 * Notifiy IO-Binding about changes to timer structure
	 * The following fields are allowed to change: enabled, interval
	 *
	 * @param  timerInfo Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*updateTimer) (ElektraIoTimerOperation * timerInfo);

	/**
	 * Remove timer from IO-Binding
	 * @param  timerInfo Information about timer
	 * @return             0 on success, any other value on error
	 */
	int (*removeTimer) (ElektraIoTimerOperation * timerInfo);

	/**
	 * Add idle to IO-Binding.
	 * Idle callbacks are executed without negative effects on other IO sources or the application (e.g. next event loop iteration)
	 *
	 * @param  binding     IO-Binding
	 * @param  idleInfo    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*addIdle) (struct ElektraIoInterface * binding, ElektraIoIdleOperation * idleInfo);

	/**
	 * Notifiy IO-Binding about changes to idle structure
	 * The following fields are allowed to change: enabled
	 *
	 * @param  idleInfo    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*updateIdle) (ElektraIoIdleOperation * idleInfo);

	/**
	 * Remove idle from IO-Binding
	 *
	 * @param  idleInfo    Information about idle
	 * @return             0 on success, any other value on error
	 */
	int (*removeIdle) (ElektraIoIdleOperation * idleInfo);

	/**
	 * Free memory used by IO-Binding.
	 * All added file descriptors and timers have to be removed before calling this function.
	 *
	 * @param  binding     IO-Binding
	 * @return             0 on success, any other value on error
	 */
	int (*cleanup) (struct ElektraIoInterface * binding);

} ElektraIoInterface;

/**
 * Create and initialize IO-Binding binding.
 * Used by elektraIoBindingTestSuite between tests to get a fresh binding instance
 * @return initialized IO-Binding binding
 */
typedef ElektraIoInterface * (*ElektraIoTestSuiteCreateBinding) (void);

/**
 * Start io processing (e.g. event loop)
 * Used by elektraIoBindingTestSuite.
 *
 * Should not return until processing is stopped (e.g. by calling ElektraIoTestSuiteStop)
 */
typedef void (*ElektraIoTestSuiteStart) (void);

/**
 * Stop IO processing (e.g. event loop)
 * Used by elektraIoTestSuite
 */
typedef void (*ElektraIoTestSuiteStop) (void);

/**
 * Test-Suite for IO-Bindings.
 *
 * @param createBinding Create and initialize a new binding instance
 * @param start         Pointer to the start function
 * @param stop          Pointer to the stop function
 */
void elektraIoTestSuite (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop);

#endif
