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
	ELEKTRA_LOG_LEVEL_WARNING = 8,
	ELEKTRA_LOG_LEVEL_NOTICE = 4,
	ELEKTRA_LOG_LEVEL_INFO = 2,
	ELEKTRA_LOG_LEVEL = ELEKTRA_LOG_LEVEL_INFO,
	ELEKTRA_LOG_LEVEL_DEBUG = 1,
};

#ifdef HAVE_LOGGER

int elektraLog (int level, const char * function, const char * file, const int line, const char * msg, ...);

#define ELEKTRA_LOG_WARNING(...) elektraLog (ELEKTRA_LOG_LEVEL_WARNING, __func__, __FILE__, __LINE__, ##__VA_ARGS__)
#define ELEKTRA_LOG_NOTICE(...) elektraLog (ELEKTRA_LOG_LEVEL_NOTICE, __func__, __FILE__, __LINE__, ##__VA_ARGS__)
#define ELEKTRA_LOG(...) elektraLog (ELEKTRA_LOG_LEVEL, __func__, __FILE__, __LINE__, ##__VA_ARGS__)
#define ELEKTRA_LOG_DEBUG(...) elektraLog (ELEKTRA_LOG_LEVEL_DEBUG, __func__, __FILE__, __LINE__, ##__VA_ARGS__)

#else

#define ELEKTRA_LOG_WARNING(...)
#define ELEKTRA_LOG_NOTICE(...)
#define ELEKTRA_LOG(...)
#define ELEKTRA_LOG_DEBUG(...)

#endif

#endif
