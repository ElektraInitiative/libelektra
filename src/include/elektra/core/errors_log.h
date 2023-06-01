/**
 * @file
 *
 * @brief Provides all macros and definitions which are used for emitting error or warnings
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_ERRORS_LOG_H
#define ELEKTRA_KDB_ERRORS_LOG_H

#ifdef ELEKTRA_HAVE_INTERNAL_LOGGER
#include <internal/utility/logger.h>
#define ELEKTRA_ERRORS_LOG(fmt, ...) elektraLog (ELEKTRA_LOG_LEVEL_INFO, __func__, __FILE__, __LINE__, fmt, __VA_ARGS__)
#else
#define ELEKTRA_ERRORS_LOG(fmt, ...)
#endif


#endif // ELEKTRA_KDB_ERRORS_LOG_H
