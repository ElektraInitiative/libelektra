/**
 * @file
 *
 * @brief Elektra-IO structures for IO bindings, plugins and applications
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IO_H_
#define KDB_IO_H_

typedef struct _ElektraIoInterface ElektraIoInterface;
typedef struct _ElektraIoFdOperation ElektraIoFdOperation;
typedef struct _ElektraIoTimerOperation ElektraIoTimerOperation;
typedef struct _ElektraIoIdleOperation ElektraIoIdleOperation;

typedef void (*ElektraIoFdCallback) (ElektraIoFdOperation * fdOp, int flags);
typedef void (*ElektraIoIdleCallback) (ElektraIoIdleOperation * idleOp);
typedef void (*ElektraIoTimerCallback) (ElektraIoTimerOperation * timerOp);

/**
 * Available flags for file descriptors
 */
typedef enum {
	ELEKTRA_IO_READABLE = 1 << 0,
	ELEKTRA_IO_WRITABLE = 1 << 1,
} ElektraIoFdFlags;

/**
 * Add file descriptor to be watched by IO-Binding.
 * An operation may only be added to one binding.
 *
 * @param  binding IO-Binding
 * @param  fdOp    file desciptor information
 * @return         0 on success, any other value on error
 */
int elektraIoBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp);

/**
 * Add file descriptor to be watched by IO-Binding.
 * An operation may only be added to one binding.
 *
 * @param  binding IO-Binding
 * @param  fdOp    file desciptor information
 * @return         0 on success, any other value on error
 */
typedef int ElektraIoBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp);

/**
 * Notify IO-Binding about changes to file descriptor watch operation.
 *
 * @param  fdOp    File descriptor information
 * @return         0 on success, any other value on error
 */
int elektraIoBindingUpdateFd (ElektraIoFdOperation * fdOp);

/**
 * Notify IO-Binding about changes to file descriptor watch operation.
 *
 * @param  fdOp    File descriptor information
 * @return         0 on success, any other value on error
 */
typedef int ElektraIoBindingUpdateFd (ElektraIoFdOperation * fdOp);

/**
 * Remove file descriptor from IO-Binding
 *
 * @param  fdOp    File descriptor information
 * @return         0 on success, any other value on error
 */
int elektraIoBindingRemoveFd (ElektraIoFdOperation * fdOp);

/**
 * Remove file descriptor from IO-Binding
 *
 * @param  fdOp    File descriptor information
 * @return         0 on success, any other value on error
 */
typedef int ElektraIoBindingRemoveFd (ElektraIoFdOperation * fdOp);

/**
 * Add timer to IO-Binding.
 * Timeouts callbacks are executed after the initial interval has elapsed and then repeatedly after the interval has elapsed.
 * An operation may only be added to one binding.
 *
 * @param  binding  IO-Binding
 * @param  timerOp  timer operation
 * @return          0 on success, any other value on error
 */
int elektraIoBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp);

/**
 * Add timer to IO-Binding.
 * Timeouts callbacks are executed after the initial interval has elapsed and then repeatedly after the interval has elapsed.
 * An operation may only be added to one binding.
 *
 * @param  binding  IO-Binding
 * @param  timerOp  timer operation
 * @return          0 on success, any other value on error
 */
typedef int ElektraIoBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp);

/**
 * Notifiy IO-Binding about changes to timer structure
 *
 * @param  timerOp  timer operation
 * @return          0 on success, any other value on error
 */
int elektraIoBindingUpdateTimer (ElektraIoTimerOperation * timerOp);

/**
 * Notifiy IO-Binding about changes to timer structure
 *
 * @param  timerOp  timer operation
 * @return          0 on success, any other value on error
 */
typedef int ElektraIoBindingUpdateTimer (ElektraIoTimerOperation * timerOp);

/**
 * Remove timer from IO-Binding
 *
 * @param  timerOp  timer operation
 * @return          0 on success, any other value on error
 */
int elektraIoBindingRemoveTimer (ElektraIoTimerOperation * timerOp);

/**
 * Remove timer from IO-Binding
 *
 * @param  timerOp  timer operation
 * @return          0 on success, any other value on error
 */
typedef int ElektraIoBindingRemoveTimer (ElektraIoTimerOperation * timerOp);

/**
 * Add idle to IO-Binding.
 * Idle callbacks are executed without negative effects on other IO sources or the application (e.g. next event loop iteration)
 * An operation may only be added to one binding.
 *
 * @param  binding  IO-Binding
 * @param  idleOp   idle operation
 * @return          0 on success, any other value on error
 */
int elektraIoBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp);

/**
 * Add idle to IO-Binding.
 * Idle callbacks are executed without negative effects on other IO sources or the application (e.g. next event loop iteration)
 * An operation may only be added to one binding.
 *
 * @param  binding  IO-Binding
 * @param  idleOp   idle operation
 * @return          0 on success, any other value on error
 */
typedef int ElektraIoBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp);

/**
 * Notifiy IO-Binding about changes to idle structure
 *
 * @param  idleOp  idle operation
 * @return          0 on success, any other value on error
 */
int elektraIoBindingUpdateIdle (ElektraIoIdleOperation * idleOp);

/**
 * Notifiy IO-Binding about changes to idle structure
 *
 * @param  idleOp  idle operation
 * @return          0 on success, any other value on error
 */
typedef int ElektraIoBindingUpdateIdle (ElektraIoIdleOperation * idleOp);

/**
 * Remove idle from IO-Binding
 *
 * @param  idleOp  idle operation
 * @return          0 on success, any other value on error
 */
int elektraIoBindingRemoveIdle (ElektraIoIdleOperation * idleOp);

/**
 * Remove idle from IO-Binding
 *
 * @param  idleOp  idle operation
 * @return          0 on success, any other value on error
 */
typedef int ElektraIoBindingRemoveIdle (ElektraIoIdleOperation * idleOp);

/**
 * Free memory used by IO-Binding.
 * All added operations have to be removed before calling this function.
 *
 * @param  binding  IO-Binding
 * @return          0 on success, any other value on error
 */
int elektraIoBindingCleanup (ElektraIoInterface * binding);

/**
 * Free memory used by IO-Binding.
 * All added operations have to be removed before calling this function.
 *
 * @param  binding  IO-Binding
 * @return          0 on success, any other value on error
 */
typedef int ElektraIoBindingCleanup (ElektraIoInterface * binding);

/**
 * Create a new IO-Binding.
 * Free returned data after usage.
 *
 * @param  addFd       function for adding a file descriptor watch operation
 * @param  updateFd    function for updating a file descriptor operation
 * @param  removeFd    function for removing a file descriptor operation
 * @param  addTimer    function for adding a timer operation
 * @param  updateTimer function for updateing a timer operation
 * @param  removeTimer function for removing a timer operation
 * @param  addIdle     function for adding an idle operation
 * @param  updateIdle  function for updating an idle operation
 * @param  removeIdle  function for removing an idle operation
 * @param  cleanup     function for cleaning up binding data
 * @return                             newly created binding
 */
ElektraIoInterface * elektraIoNewBinding (
	// file descriptor
	ElektraIoBindingAddFd * addFd, ElektraIoBindingUpdateFd * updateFd, ElektraIoBindingRemoveFd * removeFd,
	// timer
	ElektraIoBindingAddTimer * addTimer, ElektraIoBindingUpdateTimer * updateTimer, ElektraIoBindingRemoveTimer * removeTimer,
	// idle
	ElektraIoBindingAddIdle * addIdle, ElektraIoBindingUpdateIdle * updateIdle, ElektraIoBindingRemoveIdle * removeIdle,
	// cleanup
	ElektraIoBindingCleanup * cleanup);

/**
 * Get private data from IO Binding
 * To be used by bindings only
 *
 * @param  binding  IO-Binding
 * @return          pointer to data or NULL on error
 */
void * elektraIoBindingGetData (ElektraIoInterface * binding);

/**
 * Set private data from IO Binding
 * To be used by bindings only
 *
 * @param  binding  IO-Binding
 * @param  data     Private data
 * @return          0 on success, any other value on error
 */
int elektraIoBindingSetData (ElektraIoInterface * binding, void * data);

/**
 * Create a new file descriptor watch operation.
 * Free returned data after usage.
 *
 * @param  fd          file descriptor number
 * @param  flags       watch flag bitmask (@see ElektraIoFdFlags). Select on which file descriptor state changes the callback should be
 * invoked
 * @param  enabled  0 to disabled, any other value for enabled
 * @param  callback Called when file descriptor state has changes
 * @param  data     Custom private data
 * @return          file descriptor operation structure
 */
ElektraIoFdOperation * elektraIoNewFdOperation (int fd, int flags, int enabled, ElektraIoFdCallback callback, void * data);

/**
 * Enable or disable file descriptor watch operation
 * @param  fdOp    file descriptor operation
 * @param  enabled 0 to disabled, any other value for enabled
 * @return         0 on success, any other value on error
 */
int elektraIoFdSetEnabled (ElektraIoFdOperation * fdOp, int enabled);

/**
 * Update flag bitmask of file descriptor watch operation
 * @param  fdOp    file descriptor operation
 * @param  flags   watch flag bitmask (@see ElektraIoFdFlags).
 * @return         0 on success, any other value on error
 */
int elektraIoFdSetFlags (ElektraIoFdOperation * fdOp, int flags);

/**
 * Set file descriptor for file descriptor watch operation
 * @param  fdOp    file descriptor operation
 * @param  fd      file descriptor number
 * @return         0 on success, any other value on error
 */
int elektraIoFdSetFd (ElektraIoFdOperation * fdOp, int fd);

/**
 * Get file descriptor number from operation
 * @param  fdOp    file descriptor operation
 * @return         file descriptor number or -1 on error
 */
int elektraIoFdGetFd (ElektraIoFdOperation * fdOp);

/**
 * Get private data from operation
 * @param  fdOp file descriptor operation
 * @return      pointer to data or NULL on error
 */
void * elektraIoFdGetData (ElektraIoFdOperation * fdOp);

/**
 * Set private binding data for operation
 * @param  fdOp file descriptor operation
 * @param  data pointer to data
 * @return      0 on success, any other value on error
 */
int elektraIoFdSetBindingData (ElektraIoFdOperation * fdOp, void * data);

/**
 * Get private binding data from operation
 * @param  fdOp file descriptor operation
 * @return      pointer to data or NULL on error
 */
void * elektraIoFdGetBindingData (ElektraIoFdOperation * fdOp);

/**
 * Check if file descriptor watch operation is enabled or disabled
 * @param  fdOp    file descriptor operation
 * @return         0 to disabled, any other value for enabled
 */
int elektraIoFdIsEnabled (ElektraIoFdOperation * fdOp);

/**
 * Get flag bitmask of file descriptor watch operation
 * @param  fdOp    file descriptor operation
 * @return         watch flag bitmask (@see ElektraIoFdFlags).
 */
int elektraIoFdGetFlags (ElektraIoFdOperation * fdOp);

/**
 * Get callback of file descriptor watch operation
 * @param  fdOp    file descriptor operation
 * @return         callback
 */
ElektraIoFdCallback elektraIoFdGetCallback (ElektraIoFdOperation * fdOp);

/**
 * Create a new timer operation.
 * Free returned data after usage.
 *
 * @param  interval    timer interval in miliseconds
 * @param  enabled     0 to disable, any other value for enabled
 * @param  callback    Called when file descriptor state has changes
 * @param  data        Custom private data
 * @return             file descriptor operation structure
 */
ElektraIoTimerOperation * elektraIoNewTimerOperation (unsigned int interval, int enabled, ElektraIoTimerCallback callback, void * data);

/**
 * Enable or disable timer operation
 * @param  timerOp   timer operation
 * @param  enabled 0 to disabled, any other value for enabled
 * @return         0 on success, any other value on error
 */
int elektraIoTimerSetEnabled (ElektraIoTimerOperation * timerOp, int enabled);

/**
 * Check if timer operation is enabled or disabled
 * @param  timerOp   timer operation
 * @return         0 to disabled, any other value for enabled
 */
int elektraIoTimerIsEnabled (ElektraIoTimerOperation * timerOp);

/**
 * Update interval of timer operation
 * @param  timerOp  timer operation
 * @param  interval timer interval in miliseconds
 * @return          0 on success, any other value on error
 */
int elektraIoTimerSetInterval (ElektraIoTimerOperation * timerOp, unsigned int interval);

/**
 * Get interval of timer operation
 * @param  timerOp  timer operation
 * @return          timer interval in miliseconds, 0 on error
 */
unsigned int elektraIoTimerGetInterval (ElektraIoTimerOperation * timerOp);

/**
 * Set private binding data for operation
 * @param  timerOp  timer operation
 * @param  data     pointer to data
 * @return          0 on success, any other value on error
 */
int elektraIoTimerSetBindingData (ElektraIoTimerOperation * timerOp, void * data);

/**
 * Get private binding data from operation
 * @param  timerOp  timer operation
 * @return          pointer to data or NULL on error
 */
void * elektraIoTimerGetBindingData (ElektraIoTimerOperation * timerOp);

/**
 * Get private data from operation
 * @param  timerOp  timer operation
 * @return          pointer to data or NULL on error
 */
void * elektraIoTimerGetData (ElektraIoTimerOperation * timerOp);

/**
 * Get callback of timer operation
 * @param  timerOp  file descriptor operation
 * @return          callback
 */
ElektraIoTimerCallback elektraIoTimerGetCallback (ElektraIoTimerOperation * timerOp);

/**
 * Create a new idle operation.
 * Free returned data after usage.
 *
 * @param  enabled     0 to disable, any other value for enabled
 * @param  callback    Called when file descriptor state has changes
 * @param  data        Custom private data
 * @return             file descriptor operation structure
 */
ElektraIoIdleOperation * elektraIoNewIdleOperation (int enabled, ElektraIoIdleCallback callback, void * data);

/**
 * Enable or disable idle operation
 * @param  idleOp  idle operation
 * @param  enabled 0 to disabled, any other value for enabled
 * @return         0 on success, any other value on error
 */
int elektraIoIdleSetEnabled (ElektraIoIdleOperation * idleOp, int enabled);

/**
 * Check if idle operation is enabled or disabled
 * @param  idleOp   idle operation
 * @return         0 to disabled, any other value for enabled
 */
int elektraIoIdleIsEnabled (ElektraIoIdleOperation * idleOp);

/**
 * Set private binding data for operation
 * @param  idleOp   idle operation
 * @param  data     pointer to data
 * @return          0 on success, any other value on error
 */
int elektraIoIdleSetBindingData (ElektraIoIdleOperation * idleOp, void * data);

/**
 * Get private binding data from operation
 * @param  idleOp   idle operation
 * @return          pointer to data or NULL on error
 */
void * elektraIoIdleGetBindingData (ElektraIoIdleOperation * idleOp);

/**
 * Get private data from operation
 * @param  idleOp   idle operation
 * @return          pointer to data or NULL on error
 */
void * elektraIoIdleGetData (ElektraIoIdleOperation * idleOp);

/**
 * Get callback of idle operation
 * @param  idleOp   idle operation
 * @return          callback
 */
ElektraIoIdleCallback elektraIoIdleGetCallback (ElektraIoIdleOperation * idleOp);

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
