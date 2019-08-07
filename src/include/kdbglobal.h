/**
 * @file
 *
 * @brief Defines for global plugins
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBGLOBAL_H
#define ELEKTRA_KDBGLOBAL_H

#include <elektra/kdb.h>
#include <kdbmacros.h>
#include <kdbplugin.h>

/**
 * Helper for identifying global plugin positions
 *
 * We chose using these macros over other solutions
 * in order to have the array available statically.
 * Thus we can avoid initializing the KDB struct
 * during runtime and still maintain the flexibility
 * of easily adding new hook positions.
 */
// clang-format off
#define FOREACH_POSITION(POSITION) \
	POSITION(PREROLLBACK) \
	POSITION(ROLLBACK) \
	POSITION(POSTROLLBACK) \
	POSITION(GETRESOLVER) \
	POSITION(PREGETCACHE) \
	POSITION(PREGETSTORAGE) \
	POSITION(GETSTORAGE) \
	POSITION(PROCGETSTORAGE) \
	POSITION(POSTGETSTORAGE) \
	POSITION(POSTGETCACHE) \
	POSITION(SETRESOLVER) \
	POSITION(POSTGETCLEANUP) \
	POSITION(PRESETSTORAGE) \
	POSITION(SETSTORAGE) \
	POSITION(PRESETCLEANUP) \
	POSITION(PRECOMMIT) \
	POSITION(COMMIT) \
	POSITION(POSTCOMMIT) \
	POSITION(NR_GLOBAL_POSITIONS)

#define FOREACH_SUBPOSITION(SUBPOSITION) \
	SUBPOSITION(INIT) \
	SUBPOSITION(MAXONCE) \
	SUBPOSITION(FOREACH) \
	SUBPOSITION(DEINIT) \
	SUBPOSITION(NR_GLOBAL_SUBPOSITIONS)

#define GENERATE_ENUM(ENUM) ENUM,
#define GENERATE_STRING(STRING) #STRING,
// clang-format on

typedef enum
{
	FOREACH_POSITION (GENERATE_ENUM)
} GlobalpluginPositions;

typedef enum
{
	FOREACH_SUBPOSITION (GENERATE_ENUM)
} GlobalpluginSubPositions;

static const char * GlobalpluginPositionsStr[] ELEKTRA_UNUSED = { FOREACH_POSITION (GENERATE_STRING) };

static const char * GlobalpluginSubPositionsStr[] ELEKTRA_UNUSED = { FOREACH_SUBPOSITION (GENERATE_STRING) };

#endif // ELEKTRA_KDBGLOBAL_H
