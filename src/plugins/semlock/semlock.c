/**
 * @file
 *
 * @brief Source for semlock plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "semlock.h"

#include <kdberrors.h>
#include <kdbhelper.h>


typedef enum { PRE = 0, POST } State;

typedef struct
{
	sem_t * tryRead;
	sem_t * read;
	sem_t * write;
	sem_t * readCount;
	sem_t * writeCount;
	sem_t * access;
	sem_t * sem_mutex;
	State state;
} Data;

// multiple calls of sem_open ignore the mode and initval
static sem_t * openMutex (const char * name)
{
	return sem_open (name, O_CREAT, S_IRWXU | S_IRWXG | S_IRWXO, 1);
}

static sem_t * openCount (const char * name)
{
	return sem_open (name, O_CREAT, S_IRWXU | S_IRWXG | S_IRWXO, 0);
}

static void errorOpen (int error, Key * parentKey)
{
	if (error == EMFILE)
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "Open semaphore: %s\n", "EMFILE");
	}
	else if (error == ENFILE)
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "Open semaphore: %s\n", "ENFILE");
	}
	else if (error == ENOMEM)
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "Open semaphore: %s\n", "ENOMEM");
	}
	else if (error == ENOSYS)
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "Open semaphore: %s\n", "ENOSYS. /dev/shm should be mounted as tempfs. Please look in the README using \"kdb info semlock\"!");
	}
}

int elektraSemlockOpen (Plugin * handle, Key * errorKey)
{
	Data * data = elektraCalloc (sizeof (Data));
	if (!data)
	{
		ELEKTRA_SET_ERRORF (145, errorKey, "Open semaphore: %s\n", "malloc fail");
		return -1;
	}
	data->state = PRE;
	data->sem_mutex = openMutex (SEM_MUTEX);
	if (data->sem_mutex == SEM_FAILED)
	{
		int error = errno;
		errorOpen (error, errorKey);
		return -1;
	}
	sem_wait (data->sem_mutex);
	data->tryRead = openMutex (TRYREAD_MUTEX);
	data->read = openMutex (READ_MUTEX);
	data->write = openMutex (WRITE_MUTEX);
	data->access = openMutex (ACCESS_MUTEX);
	data->readCount = openCount (READ_COUNT);
	data->writeCount = openCount (WRITE_COUNT);
	sem_post (data->sem_mutex);
	if (data->tryRead == SEM_FAILED || data->read == SEM_FAILED || data->write == SEM_FAILED || data->access == SEM_FAILED ||
	    data->readCount == SEM_FAILED || data->writeCount == SEM_FAILED)
	{
		int error = errno;
		errorOpen (error, errorKey);
		return -1;
	}
	elektraPluginSetData (handle, data);
	return 1;
}

int elektraSemlockClose (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	Data * data = elektraPluginGetData (handle);
	if (!data)
	{
		return -1;
	}
	sem_close (data->readCount);
	sem_close (data->writeCount);
	sem_close (data->read);
	sem_close (data->write);
	sem_close (data->tryRead);
	sem_close (data->access);
	sem_wait (data->sem_mutex);
	sem_unlink (READ_COUNT);
	sem_unlink (WRITE_COUNT);
	sem_unlink (READ_MUTEX);
	sem_unlink (WRITE_MUTEX);
	sem_unlink (TRYREAD_MUTEX);
	sem_unlink (ACCESS_MUTEX);
	sem_post (data->sem_mutex);
	sem_close (data->sem_mutex);
	sem_unlink (SEM_MUTEX);
	elektraFree (data);
	return 1;
}

int elektraSemlockGet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/semlock"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/semlock", KEY_VALUE, "semlock plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/semlock/exports", KEY_END),
			       keyNew ("system/elektra/modules/semlock/exports/open", KEY_FUNC, elektraSemlockOpen, KEY_END),
			       keyNew ("system/elektra/modules/semlock/exports/close", KEY_FUNC, elektraSemlockClose, KEY_END),
			       keyNew ("system/elektra/modules/semlock/exports/get", KEY_FUNC, elektraSemlockGet, KEY_END),
			       keyNew ("system/elektra/modules/semlock/exports/set", KEY_FUNC, elektraSemlockSet, KEY_END),
#include ELEKTRA_README (semlock)
			       keyNew ("system/elektra/modules/semlock/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	Data * data = elektraPluginGetData (handle);
	if (!data->state)
	{
		// PRE
		data->state = POST;
		sem_wait (data->tryRead);
		sem_wait (data->read);
		sem_post (data->readCount);
		int count;
		sem_getvalue (data->readCount, &count);
		if (count == 1)
		{
			sem_wait (data->access);
		}
		sem_post (data->read);
		sem_post (data->tryRead);
	}
	else
	{
		// POST
		data->state = PRE;
		sem_wait (data->read);
		// can never block
		sem_wait (data->readCount);
		int count;
		sem_getvalue (data->readCount, &count);
		if (count == 0)
		{
			sem_post (data->access);
		}
		sem_post (data->read);
	}
	return 1; // success
}

int elektraSemlockSet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	Data * data = elektraPluginGetData (handle);
	if (!data->state)
	{ // PRE
		data->state = POST;
		sem_wait (data->write);
		sem_post (data->writeCount);
		int count;
		sem_getvalue (data->writeCount, &count);
		if (count == 1)
		{
			sem_wait (data->tryRead);
		}
		sem_post (data->write);
		sem_wait (data->access);
	}
	else
	{
		// POST
		data->state = PRE;
		sem_post (data->access);
		sem_wait (data->write);
		// can never block
		sem_wait (data->writeCount);
		int count;
		sem_getvalue (data->writeCount, &count);
		if (count == 0)
		{
			sem_post (data->tryRead);
		}
		sem_post (data->write);
	}
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (semlock)
{
	// clang-format off
	return elektraPluginExport ("semlock",
		ELEKTRA_PLUGIN_OPEN,	&elektraSemlockOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraSemlockClose,
		ELEKTRA_PLUGIN_GET,	&elektraSemlockGet,
		ELEKTRA_PLUGIN_SET,	&elektraSemlockSet,
		ELEKTRA_PLUGIN_END);
}
