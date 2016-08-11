/**
 * @file
 *
 * @brief Logger Interface
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDBLOGGER_H
#define KDBLOGGER_H

#include "kdbconfig.h"

enum ElektraLogLevel
{
	ELEKTRA_LOG_LEVEL_WARNING,
	ELEKTRA_LOG_LEVEL_NOTICE,
	ELEKTRA_LOG_LEVEL_INFO,
	ELEKTRA_LOG_LEVEL = ELEKTRA_LOG_LEVEL_INFO,
	ELEKTRA_LOG_LEVEL_DEBUG,
};

enum ElektraLogModule
{
	ELEKTRA_LOG_MODULE_NONE = 0,
	ELEKTRA_LOG_MODULE_ASSERT,

	// core
	ELEKTRA_LOG_MODULE_KDB,

	// warnings+error
	ELEKTRA_LOG_MODULE_ERROR,
	ELEKTRA_LOG_MODULE_WARNINGS,

	// plugins
	ELEKTRA_LOG_MODULE_DUMP,
	ELEKTRA_LOG_MODULE_RESOLVER,
};

#ifdef HAVE_LOGGER

int elektraLog (int module, int level, const char * function, const char * file, const int line, const char * msg, ...);

#define ELEKTRA_LOG_WARNING(module, msg, ...)                                                                                              \
	elektraLog (module, ELEKTRA_LOG_LEVEL_WARNING, __func__, __FILE__, __LINE__, msg, ##__VA_ARGS__)
#define ELEKTRA_LOG_NOTICE(module, msg, ...) elektraLog (module, ELEKTRA_LOG_LEVEL_NOTICE, __func__, __FILE__, __LINE__, msg, ##__VA_ARGS__)
#define ELEKTRA_LOG(module, msg, ...) elektraLog (module, ELEKTRA_LOG_LEVEL, __func__, __FILE__, __LINE__, msg, ##__VA_ARGS__)
#define ELEKTRA_LOG_DEBUG(module, msg, ...) elektraLog (module, ELEKTRA_LOG_LEVEL_DEBUG, __func__, __FILE__, __LINE__, msg, ##__VA_ARGS__)

#else

#define ELEKTRA_LOG_WARNING(module, msg, ...)
#define ELEKTRA_LOG_NOTICE(module, msg, ...)
#define ELEKTRA_LOG(module, msg, ...)
#define ELEKTRA_LOG_DEBUG(module, msg, ...)

#endif

#endif
