/**
 * @file
 *
 * @brief Header for template plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_BACKENDPRIVATE_H
#define ELEKTRA_BACKENDPRIVATE_H

#include <kdbprivate.h>

typedef struct _BackendHandle BackendHandle;
typedef struct _Slot Slot;

struct _Slot
{
	Plugin * value;
	Slot * next;
};

/**
 * Holds all information related to a backend.
 *
 * Since Elektra 0.8 a Backend consists of many plugins.
 * A backend is responsible for everything related to the process
 * of writing out or reading in configuration.
 *
 * So this holds a list of set and get plugins.
 *
 * Backends are put together through the configuration
 * in system/elektra/mountpoints
 *
 * See kdb mount tool to mount new backends.
 *
 * To develop a backend you have first to develop plugins and describe
 * through dependencies how they belong together.
 *
 * @ingroup backend
 */
struct _BackendHandle
{
	Key * mountpoint; /*!< The mountpoint where the backend resides.
	  The keyName() is the point where the backend was mounted.
	  The keyValue() is the name of the backend without pre/postfix, e.g.
	  filesys. */

	Slot * setplugins[NR_OF_SET_PLUGINS];
	Slot * getplugins[NR_OF_GET_PLUGINS];
	Slot * errorplugins[NR_OF_ERROR_PLUGINS];

	ssize_t getposition;

	ssize_t setposition;

	ssize_t errorposition;
};

#endif //ELEKTRA_BACKENDPRIVATE_H
