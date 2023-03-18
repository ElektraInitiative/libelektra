/**
 * @file
 *
 * @brief Elektra-I/O structures for I/O bindings, plugins and applications
 *
 * @ingroup kdbio
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IO_H_
#define KDB_IO_H_

#include <elektra/kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/** I/O binding handle */
typedef struct _ElektraIoInterface ElektraIoInterface;

/** file descriptor watch operation handle */
typedef struct _ElektraIoFdOperation ElektraIoFdOperation;

/** timer operation handle */
typedef struct _ElektraIoTimerOperation ElektraIoTimerOperation;

/** idle operation handle */
typedef struct _ElektraIoIdleOperation ElektraIoIdleOperation;

/**
 * Callback for file descriptor watch operations.
 *
 * Called whenever a file descriptor status changes.
 *
 * @param  fdOp   operation handle
 * @param  flags  current file descriptor flag bitmask
 */
typedef void (*ElektraIoFdCallback) (ElektraIoFdOperation * fdOp, int flags);

/**
 * Callback for idle operations.
 *
 * Called whenever compute intensive tasks can be execute without interfering
 * with other operations.
 *
 * @param  idleOp   operation handle
 */
typedef void (*ElektraIoIdleCallback) (ElektraIoIdleOperation * idleOp);

/**
 * Callback for timer operations.
 *
 * Called after the timer interval has elapsed.
 *
 * @param  timerOp  operation handle
 */
typedef void (*ElektraIoTimerCallback) (ElektraIoTimerOperation * timerOp);

/**
 * Available flags for file descriptors operation bitmask
 */
typedef enum
{

	/** file descriptor is readable */
	ELEKTRA_IO_READABLE = 1 << 0,

	/** file descriptor is writable */
	ELEKTRA_IO_WRITABLE = 1 << 1,

} ElektraIoFdFlags;

/**
 * Add file descriptor to be watched by I/O binding.
 *
 * An operation may only be added to one binding.
 *
 * @param  binding I/O binding handle
 * @param  fdOp    file descriptor operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp);

/**
 * Add file descriptor to be watched by I/O binding.
 *
 * An operation may only be added to one binding.
 *
 * @param  binding I/O binding handle
 * @param  fdOp    file descriptor operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp);

/**
 * Notify I/O binding about changes to file descriptor watch operation.
 *
 * @param  fdOp    file descriptor operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingUpdateFd (ElektraIoFdOperation * fdOp);

/**
 * Notify I/O binding about changes to file descriptor watch operation.
 *
 * @param  fdOp    file descriptor operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingUpdateFd (ElektraIoFdOperation * fdOp);

/**
 * Remove file descriptor from I/O binding.
 *
 * @param  fdOp    file descriptor operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingRemoveFd (ElektraIoFdOperation * fdOp);

/**
 * Remove file descriptor from I/O binding.
 *
 * @param  fdOp    file descriptor operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingRemoveFd (ElektraIoFdOperation * fdOp);

/**
 * Add timer to I/O binding.
 *
 * Timeouts callbacks are executed after the initial interval has elapsed and then repeatedly after the interval has elapsed.
 * An operation may only be added to one binding.
 *
 * @param  binding  I/O binding handle
 * @param  timerOp  timer operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp);

/**
 * Add timer to I/O binding.
 *
 * Timeouts callbacks are executed after the initial interval has elapsed and then repeatedly after the interval has elapsed.
 * An operation may only be added to one binding.
 *
 * @param  binding  I/O binding handle
 * @param  timerOp  timer operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp);

/**
 * Notifiy I/O binding about changes to timer structure.
 *
 * @param  timerOp  timer operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingUpdateTimer (ElektraIoTimerOperation * timerOp);

/**
 * Notifiy I/O binding about changes to timer structure.
 *
 * @param  timerOp  timer operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingUpdateTimer (ElektraIoTimerOperation * timerOp);

/**
 * Remove timer from I/O binding.
 *
 * @param  timerOp  timer operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingRemoveTimer (ElektraIoTimerOperation * timerOp);

/**
 * Remove timer from I/O binding.
 *
 * @param  timerOp  timer operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingRemoveTimer (ElektraIoTimerOperation * timerOp);

/**
 * Add idle to I/O binding.
 *
 * Idle callbacks are executed without negative effects on other IO sources or the application (e.g. next event loop iteration)
 * An operation may only be added to one binding.
 *
 * @param  binding  I/O binding handle
 * @param  idleOp   idle operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp);

/**
 * Add idle to I/O binding.
 *
 * Idle callbacks are executed without negative effects on other IO sources or the application (e.g. next event loop iteration)
 * An operation may only be added to one binding.
 *
 * @param  binding  I/O binding handle
 * @param  idleOp   idle operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp);

/**
 * Notifiy I/O binding about changes to idle structure.
 *
 * @param  idleOp  idle operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingUpdateIdle (ElektraIoIdleOperation * idleOp);

/**
 * Notifiy I/O binding about changes to idle structure.
 *
 * @param  idleOp  idle operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingUpdateIdle (ElektraIoIdleOperation * idleOp);

/**
 * Remove idle from I/O binding.
 *
 * @param  idleOp  idle operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingRemoveIdle (ElektraIoIdleOperation * idleOp);

/**
 * Remove idle from I/O binding.
 *
 * @param  idleOp  idle operation handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingRemoveIdle (ElektraIoIdleOperation * idleOp);

/**
 * Free memory used by I/O binding.
 *
 * All added operations have to be removed before calling this function.
 *
 * @param  binding  I/O binding handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingCleanup (ElektraIoInterface * binding);

/**
 * Free memory used by I/O binding.
 *
 * All added operations have to be removed before calling this function.
 *
 * @param  binding  I/O binding handle
 * @retval 1 on success
 * @retval 0 on error
 */
typedef int ElektraIoBindingCleanup (ElektraIoInterface * binding);

/**
 * Create a new I/O binding.
 *
 * Make sure to free returned data in ::ElektraIoBindingCleanup.
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
 * Get private data from I/O Binding.
 *
 * To be used by I/O binding implementations only.
 *
 * @param  binding  I/O-Binding handle
 * @return          pointer to data or NULL on error
 */
void * elektraIoBindingGetData (ElektraIoInterface * binding);

/**
 * Set private data from I/O Binding.
 *
 * To be used by I/O binding implementations only.
 *
 * @param  binding  I/O binding handle
 * @param  data     private data
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoBindingSetData (ElektraIoInterface * binding, void * data);

/**
 * Create a new file descriptor watch operation.
 *
 * Free returned data after use.
 *
 * @param  fd          file descriptor number
 * @param  flags       watch flag bitmask (see ::ElektraIoFdFlags). Select on which file descriptor state changes the callback should be
 * invoked
 * @param  enabled  0 to disabled, any other value for enabled
 * @param  callback Called when file descriptor state has changes
 * @param  data     Custom private data
 * @return          file descriptor operation handle
 */
ElektraIoFdOperation * elektraIoNewFdOperation (int fd, int flags, int enabled, ElektraIoFdCallback callback, void * data);

/**
 * Enable or disable file descriptor watch operation.
 *
 * @param  fdOp    file descriptor operation handle
 * @param  enabled 0 to disabled, any other value for enabled
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoFdSetEnabled (ElektraIoFdOperation * fdOp, int enabled);

/**
 * Update flag bitmask of file descriptor watch operation.
 *
 * @param  fdOp    file descriptor operation handle
 * @param  flags   watch flag bitmask (see ::ElektraIoFdFlags).
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoFdSetFlags (ElektraIoFdOperation * fdOp, int flags);

/**
 * Get file descriptor number from operation.
 *
 * @param  fdOp    file descriptor operation handle
 * @return         file descriptor number or 0 on error
 */
int elektraIoFdGetFd (ElektraIoFdOperation * fdOp);

/**
 * Get private data from operation.
 *
 * @param  fdOp file descriptor operation handle
 * @return      pointer to data or NULL on error
 */
void * elektraIoFdGetData (ElektraIoFdOperation * fdOp);

/**
 * Set private binding data for operation.
 *
 * @param  fdOp file descriptor operation handle
 * @param  data pointer to data
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoFdSetBindingData (ElektraIoFdOperation * fdOp, void * data);

/**
 * Get private binding data from operation.
 *
 * @param  fdOp file descriptor operation handle
 * @return      pointer to data or NULL on error
 */
void * elektraIoFdGetBindingData (ElektraIoFdOperation * fdOp);

/**
 * Get binding from operation.
 *
 * @param  fdOp fd operation handle
 * @return      pointer to binding or NULL on error
 */
ElektraIoInterface * elektraIoFdGetBinding (ElektraIoFdOperation * fdOp);

/**
 * Check if file descriptor watch operation is enabled or disabled.
 *
 * @param  fdOp    file descriptor operation handle
 * @retval 0 if disabled
 * @retval 1 if enabled
 */
int elektraIoFdIsEnabled (ElektraIoFdOperation * fdOp);

/**
 * Get flag bitmask of file descriptor watch operation.
 *
 * @param  fdOp    file descriptor operation handle
 * @return         watch flag bitmask (see ::ElektraIoFdFlags).
 */
int elektraIoFdGetFlags (ElektraIoFdOperation * fdOp);

/**
 * Get callback of file descriptor watch operation.
 *
 * @param  fdOp    file descriptor operation handle
 * @return         callback
 */
ElektraIoFdCallback elektraIoFdGetCallback (ElektraIoFdOperation * fdOp);

/**
 * Create a new timer operation.
 *
 * Free returned data after use.
 *
 * @param  interval    timer interval in milliseconds
 * @param  enabled     0 to disable, any other value for enabled
 * @param  callback    Called when file descriptor state has changes
 * @param  data        Custom private data
 * @return             timer operation handle
 */
ElektraIoTimerOperation * elektraIoNewTimerOperation (unsigned int interval, int enabled, ElektraIoTimerCallback callback, void * data);

/**
 * Enable or disable timer operation.
 *
 * @param  timerOp   timer operation handle
 * @param  enabled 0 to disabled, any other value for enabled
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoTimerSetEnabled (ElektraIoTimerOperation * timerOp, int enabled);

/**
 * Check if timer operation is enabled or disabled.
 *
 * @param  timerOp   timer operation handle
 * @retval 0 if disabled
 * @retval 1 if enabled
 */
int elektraIoTimerIsEnabled (ElektraIoTimerOperation * timerOp);

/**
 * Update interval of timer operation.
 *
 * @param  timerOp  timer operation handle
 * @param  interval timer interval in milliseconds
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoTimerSetInterval (ElektraIoTimerOperation * timerOp, unsigned int interval);

/**
 * Get interval of timer operation.
 *
 * @param  timerOp  timer operation handle
 * @return          timer interval in milliseconds, 0 on error
 */
unsigned int elektraIoTimerGetInterval (ElektraIoTimerOperation * timerOp);

/**
 * Set private binding data for operation.
 *
 * @param  timerOp  timer operation handle
 * @param  data     pointer to data
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoTimerSetBindingData (ElektraIoTimerOperation * timerOp, void * data);

/**
 * Get private binding data from operation.
 *
 * @param  timerOp  timer operation handle
 * @return          pointer to data or NULL on error
 */
void * elektraIoTimerGetBindingData (ElektraIoTimerOperation * timerOp);

/**
 * Get binding from operation.
 *
 * @param  timerOp  timer operation handle
 * @return          pointer to binding or NULL on error
 */
ElektraIoInterface * elektraIoTimerGetBinding (ElektraIoTimerOperation * timerOp);

/**
 * Get private data from operation.
 *
 * @param  timerOp  timer operation handle
 * @return          pointer to data or NULL on error
 */
void * elektraIoTimerGetData (ElektraIoTimerOperation * timerOp);

/**
 * Get callback of timer operation.
 *
 * @param  timerOp  timer operation handle
 * @return          callback
 */
ElektraIoTimerCallback elektraIoTimerGetCallback (ElektraIoTimerOperation * timerOp);

/**
 * Create a new idle operation.
 *
 * Free returned data after use.
 *
 * @param  enabled     0 to disable, any other value for enabled
 * @param  callback    Called when file descriptor state has changes
 * @param  data        Custom private data
 * @return             idle operation handle
 */
ElektraIoIdleOperation * elektraIoNewIdleOperation (int enabled, ElektraIoIdleCallback callback, void * data);

/**
 * Enable or disable idle operation.
 *
 * @param  idleOp  idle operation handle
 * @param  enabled 0 to disabled, any other value for enabled
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoIdleSetEnabled (ElektraIoIdleOperation * idleOp, int enabled);

/**
 * Check if idle operation is enabled or disabled.
 *
 * @param  idleOp   idle operation handle
 * @retval 0 if disabled
 * @retval 1 if enabled
 */
int elektraIoIdleIsEnabled (ElektraIoIdleOperation * idleOp);

/**
 * Set private binding data for operation.
 *
 * @param  idleOp   idle operation handle
 * @param  data     pointer to data
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoIdleSetBindingData (ElektraIoIdleOperation * idleOp, void * data);

/**
 * Get private binding data from operation.
 *
 * @param  idleOp   idle operation handle
 * @return          pointer to data or NULL on error
 */
void * elektraIoIdleGetBindingData (ElektraIoIdleOperation * idleOp);

/**
 * Get binding from operation.
 *
 * @param  idleOp idle operation handle
 * @return        pointer to binding or NULL on error
 */
ElektraIoInterface * elektraIoIdleGetBinding (ElektraIoIdleOperation * idleOp);

/**
 * Get private data from operation.
 *
 * @param  idleOp   idle operation handle
 * @return          pointer to data or NULL on error
 */
void * elektraIoIdleGetData (ElektraIoIdleOperation * idleOp);

/**
 * Get callback of idle operation.
 *
 * @param  idleOp   idle operation handle
 * @return          callback
 */
ElektraIoIdleCallback elektraIoIdleGetCallback (ElektraIoIdleOperation * idleOp);

/**
 * Creates a contract for use with kdbOpen() that sets up an I/O binding.
 *
 * When you call kdbOpen() with this contract, the KDB instance will use
 * @p ioBinding as its I/O binding.
 *
 * @param contract  The keyset into which the contract is written.
 * @param ioBinding The ioBinding to use.
 *
 * @retval -1 if @p contract or @p ioBinding are NULL
 * @retval  0 on success
 */
int elektraIoContract (KeySet * contract, ElektraIoInterface * ioBinding);

/**
 * Get I/O binding for asynchronous I/O operations for KDB instance.
 * Returns NULL if no I/O binding was set.
 *
 * @param  kdb KDB instance
 * @return I/O binding or NULL
 */
ElektraIoInterface * elektraIoGetBinding (KDB * kdb);

#ifdef __cplusplus
}
}
#endif

#endif
