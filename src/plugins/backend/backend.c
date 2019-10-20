

#include "backend.h"

#include <kdberrors.h>
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

	ssize_t specsize;	/*!< The size of the spec key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */
	ssize_t dirsize;	/*!< The size of the dir key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */
	ssize_t usersize;	/*!< The size of the users key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */
	ssize_t systemsize; /*!< The size of the systems key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */

	size_t refcounter; /*!< This refcounter shows how often the backend
	   is used.  Not cascading or default backends have 1 in it.
	   More than three is not possible, because a backend
	   can be only mounted in dir, system and user each once
	   OR only in spec.*/
};

int setMountpoint (BackendHandle * bh, KeySet * config, Key * errorKey)
{
	Key * root;

	ksRewind (config);

	root = ksNext (config);

	Key * searchMountpoint = keyDup (root);
	keyAddBaseName (searchMountpoint, "mountpoint");
	Key * foundMountpoint = ksLookup (config, searchMountpoint, 0);

	keyDel (searchMountpoint);

	if (!foundMountpoint)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not find mountpoint within root %s", keyName (root));
		return -1;
	}

	bh->mountpoint = keyNew ("", KEY_VALUE, keyBaseName (root), KEY_END);
	elektraKeySetName (bh->mountpoint, keyString (foundMountpoint), KEY_CASCADING_NAME | KEY_EMPTY_NAME);

	keySetName (errorKey, keyName (bh->mountpoint));

	if(!bh->mountpoint)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not create mountpoint with name '%s' and value %s",
						   keyString (foundMountpoint), keyBaseName (root));
		return -1;
	}

	keyIncRef (bh->mountpoint);
	return 0;
}

KeySet * processConfig(BackendHandle * bh, KeySet * config, Key * errorKey)
{
	if(setMountpoint (bh, config, errorKey) == -1)
	{
		return NULL;
	}

	KeySet * systemConfig = elektraRenameKeys (config, "system");
	ksDel (config);

	return systemConfig;
}

int linkedListPosition (Key * cur, Key * errorKey)
{
	const char * name = keyBaseName (cur);

	if (name[0] != '#')
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Plugin positions must start with a #. Position: %s", name);
		return -1;
	}

	int numberOfDigits = 1;

	while (name[numberOfDigits] == '_')
	{
		numberOfDigits++;
	}

	int position = 0;

	int digitCountdown = numberOfDigits;

	while (digitCountdown != 0)
	{
		position *= 10;

		if(name[numberOfDigits] < '0' || name[numberOfDigits] > '9')
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (
				errorKey, "Plugin positions must be numbers. Position: %s", name);
			return -1;
		}

		position += name[numberOfDigits] - '0';

		numberOfDigits++;
		digitCountdown--;
	}


	return position;
}

int processPlugin(Key * config, Key * cur, char ** name, KeySet ** pluginConfig, char ** referenceName, Key * errorKey)
{
	*name = keyBaseName (cur);

	// This key will be used to find the configuration of the plugin, if it exists
	Key * pluginConfigSearchKey = keyDup (cur);
	keyAddBaseName (pluginConfigSearchKey, "config");

	// This key will be used to find the plugin label, which it can be referenced with afterwards, if it exists
	Key * labelSearchKey = keyDup (cur);
	keyAddBaseName (labelSearchKey, "label");

	// This key will be used to find the name of the plugin
	Key * nameSearchKey = keyDup (cur);
	keyAddBaseName (nameSearchKey, "name");

	// This key will be used to find the plugin's reference name, which is the label it was first set with
	Key * refSearchKey = keyDup (cur);
	keyAddBaseName (refSearchKey, "ref");

	KeySet * cutPluginConfig = ksCut (config, pluginConfigSearchKey);
	keyDel (pluginConfigSearchKey);

	*pluginConfig = elektraRenameKeys (cutPluginConfig, "user");
	ksDel (cutPluginConfig);

	Key * labelKey = ksLookup (config, labelSearchKey, KDB_O_POP);

	Key * nameKey = ksLookup (config, nameSearchKey, KDB_O_POP);

	Key * refKey = ksLookup (config, refSearchKey, KDB_O_POP);

	if (labelKey == -1)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey,"Could not lookup the key: %s", labelSearchKey);
		return -1;
	}
	keyDel (labelSearchKey);

	if (nameKey == -1)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey,"Could not lookup the key: %s", nameSearchKey);
		return -1;
	}
	keyDel (nameSearchKey);

	if (refKey == -1)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey,"Could not lookup the key: %s", refSearchKey);
		return -1;
	}
	keyDel (refSearchKey);

	if (refKey != 0)
	{
		//An existing plugin will be used
		if (labelKey != 0)
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "The label and reference name of a plugin cannot be defined at the same time! Label: %s Reference name: %s", labelKey, refKey);
			return -1;
		}

		if (nameKey != 0)
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "The plugin name and reference name of a plugin cannot be defined at the same time! Plugin name: %s, Reference name: %s", nameKey, refKey);
			return -1;
		}

		char prefixReferenceName[] = "system/elektra/plugins/";
		char keyName = keyString (refKey);

		*referenceName = elektraMalloc (sizeof (prefixReferenceName) + sizeof (keyName) -1);
		strcpy (*referenceName, prefixReferenceName);
		strcat (*referenceName, keyName);

		return 2;
	}
	else if (nameKey != 0) //A new plugin will be created
	{
		*name = keyString (nameKey);
		if (labelKey != 0)
		{
			//A label will be defined for later referencing
			*referenceName = keyString (labelKey);
			return 3;
		}
		return 1;
	}
	else if (labelKey != 0)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "The plugin name must be defined If the label is defined! Plugin name: %s, Label: %s", nameKey, labelKey);
		return -1;
	}
	else
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (errorKey, "Plugin name, reference name and label are not defined!");
		return -1;
	}
}

Slot * processRole (KeySet * config, KeySet * modules, KeySet * referencePlugins, KeySet * systemConfig, Key * errorKey)
{
	Key * root;
	Key * cur;

	ksRewind (config);

	root = ksNext (config);

	Slot * slot = 0;

	while ((cur = ksNext (config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			int position;

			if((position = linkedListPosition (cur,errorKey)) == -1)
			{
				ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey,"Could not parse the position of the plugin in the linked list from the key: %s", cur);
				return 0;
			}

			char * name = 0;

			KeySet * cut = ksCut (config, cur);

			KeySet * pluginConfig;

			char * referenceName = 0;

			int ret = processPlugin (cut, cur, &name, &pluginConfig, &referenceName, errorKey);

			if(ret == -1)
			{
				ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not parse the name, label and configuration of the plugin from the keyset: %s", cut);
				return NULL;
			}

			Slot * curSlot = slot;

			for (int a = 0; a <= position; a++)
			{
				//Allocate new slot if it is null
				if (!curSlot)
				{
					curSlot = elektraMalloc (sizeof (Slot));
				}

				//Insert the plugin into its slot
				if (a == position)
				{
					if (name)
					{
						ksAppend (pluginConfig, systemConfig);
						ksRewind (pluginConfig);

						curSlot->value = elektraPluginOpen (name, modules, pluginConfig, errorKey);

						if (!curSlot->value)
						{
							ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not load plugin %s", name);
							elektraFree (name);

							return 0;
						}

						//TODO set global keyset

						if (referenceName)
						{
							ksAppendKey (referencePlugins,
								     keyNew (referenceName, KEY_BINARY, KEY_SIZE, sizeof (curSlot->value), KEY_VALUE,
									     &curSlot->value, KEY_END));
						}
					}
					else
					{
						Key * lookup = ksLookup (referencePlugins, keyNew (referenceName, KEY_END), KDB_O_DEL);
						if (!lookup)
						{
							ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not reference back to plugin %s", referenceName);
							elektraFree (referenceName);
							ksDel (config);
							return 0;
						}

						curSlot->value = *(Plugin **) keyValue (lookup);
						++curSlot->value->refcounter;
					}
				}
				else
				{
					curSlot = curSlot->next;
				}
			}

			elektraFree (name);
			elektraFree (referenceName);
		}
		else
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Unknown entries in plugin configuration: %s",
							   keyString (cur));
		}
	}

	ksDel (config);

	if (!slot)
	{
		slot = elektraMalloc (sizeof (Slot));
	}

	return slot;
}

Slot ** processGetPlugins (KeySet * modules, KeySet * referencePlugins, KeySet * config, KeySet * systemConfig, Key * errorKey)
{
	Key * root;
	Key * cur;
	Slot ** slots;

	ksRewind (config);

	root = ksNext (config);

	while ((cur = ksNext (config)))
	{
		if (keyRel (root,cur) == 1)
		{
			KeySet * cut = ksCut (config, cur);
			Slot * slot;
			if (!strcmp (keyBaseName (cur), "getresolver"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[0] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "pregetstorage"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[1] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "getstorage"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[2] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "postgetstorage"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[3] = slot;
			}
			else
			{
				ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Unexpected key: %s", cur);
				return 0;
			}
		}
	}

	return slots;
}

Slot ** processSetPlugins (KeySet * modules, KeySet * referencePlugins, KeySet * config, KeySet * systemConfig, Key * errorKey)
{
	Key * root;
	Key * cur;
	Slot ** slots;

	ksRewind (config);

	root = ksNext (config);

	while ((cur = ksNext (config)) != 0)
	{
		if (keyRel (root,cur) == 1)
		{
			KeySet * cut = ksCut (config, cur);
			Slot * slot;
			if (!strcmp (keyBaseName (cur), "setresolver"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[0] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "presetstorage"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[1] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "setstorage"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[2] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "precommit"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[3] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "commit"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[4] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "postcommit"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not build up slots with the configuration: %s", cut);
					return 0;
				}

				slots[5] = slot;
			}
			else
			{
				ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Unexpected key: %s", cur);
				return 0;
			}
		}
	}

	return slots;
}

Slot ** processErrorPlugins (KeySet * modules, KeySet * referencePlugins, KeySet * config, KeySet * systemConfig, Key * errorKey)
{
	Key * root;
	Key * cur;
	Slot ** slots;

	ksRewind (config);

	root = ksNext (config);

	while ((cur = ksNext (config)) != 0)
	{
		if (keyRel (root,cur) == 1)
		{
			KeySet * cut = ksCut (config, cur);
			Slot * slot;
			if (!strcmp (keyBaseName (cur), "prerollback"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (errorKey, "Could not build up prerollback slots");
					return 0;
				}

				slots[0] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "rollback"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (errorKey, "Could not build up rollback slots");
					return 0;
				}

				slots[1] = slot;
			}
			else if (!strcmp (keyBaseName (cur), "postrollback"))
			{
				slot = processRole (cut, modules, referencePlugins, systemConfig, errorKey);

				if (slot == 0)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (errorKey, "Could not build up postrollback slots");
					return 0;
				}

				slots[2] = slot;
			}
			else
			{
				ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Unexpected key: %s", keyName (cur));
				return 0;
			}
		}
	}

	return slots;
}

int elektraBackendOpen (Plugin * handle, Key * errorKey)
{
	BackendHandle * bh = elektraMalloc (sizeof (BackendHandle));

	ksRewind (handle->config);

	Key * root = ksNext (handle->config);

	KeySet * systemConfig = 0;
	KeySet * referencePlugins = ksNew (0, KS_END);

	Key * configKey = keyDup (root);
	keyAddBaseName (configKey, "config");
	KeySet * configSet = ksCut (handle->config, configKey);
	ksDel (configKey);

	systemConfig = processConfig (bh, configSet, errorKey);

	if(systemConfig == 0)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (
			errorKey, "Could not generate system config");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * getPluginsKey = keyDup (root);
	keyAddBaseName (getPluginsKey, "get");
	KeySet * getPluginsSet = ksCut (handle->config, getPluginsKey);
	ksDel (getPluginsKey);

	Slot ** getPlugins = processGetPlugins (handle->modules, referencePlugins, getPluginsSet, systemConfig, errorKey);

	if (!getPlugins)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (errorKey, "Could not build up get array");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	for (int a = 0; a < NR_OF_GET_PLUGINS; a++)
	{
		bh->getplugins[a] = getPlugins[a];
	}

	ksDel (getPluginsSet);

	Key * setPluginsKey = keyDup (root);
	keyAddBaseName (setPluginsKey, "set");
	KeySet * setPluginsSet = ksCut (handle->config, setPluginsKey);
	ksDel (setPluginsKey);

	Slot ** setPlugins = processSetPlugins (handle->modules, referencePlugins, setPluginsSet, systemConfig, errorKey);

	if (!setPlugins)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (errorKey, "Could not build up set array");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	for (int a = 0; a < NR_OF_SET_PLUGINS; a++)
	{
		bh->setplugins[a] = setPlugins[a];
	}

	ksDel (setPluginsSet);

	Key * errorPluginsKey = keyDup (root);
	keyAddBaseName (errorPluginsKey, "error");
	KeySet * errorPluginsSet = ksCut (handle->config, errorPluginsKey);
	ksDel (errorPluginsKey);

	Slot ** errorPlugins = processErrorPlugins (handle->modules, referencePlugins, errorPluginsSet, systemConfig, errorKey);

	if (!errorPlugins)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (errorKey, "Could not build up error array");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	for (int a = 0; a < NR_OF_ERROR_PLUGINS; a++)
	{
		bh->errorplugins[a] = errorPlugins[a];
	}

	ksDel (errorPluginsSet);

	//TODO Open missing backend instead of returning errors
	//TODO set global keyset

	elektraPluginSetData (handle, bh);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendClose (Plugin * handle, Key * errorKey)
{
	BackendHandle * bh = elektraPluginGetData (handle);
	if (bh)
	{
		elektraFree (bh);
		elektraPluginSetData (handle, 0);
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendGet (Plugin * handle, KeySet * ks, Key * parentKey)
{
	//TODO implement global plugins

	BackendHandle * bh = elektraPluginGetData (handle);

	if (!bh)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "The backend handle is not defined!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Slot * resolver = bh->getplugins[GET_RESOLVER];

	if (!resolver || !resolver->value || !resolver->value->kdbGet)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "The resolver is not defined properly, the backend was not initialized correctly!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int returnValue = resolver->value->kdbGet (handle, ks, parentKey);

	switch (returnValue) {
	case ELEKTRA_PLUGIN_STATUS_CACHE_HIT:
		return returnValue;
	case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
		return returnValue;
	case ELEKTRA_PLUGIN_STATUS_ERROR:
		return returnValue;
	}

	for (int a = 1; a < NR_OF_GET_PLUGINS; a++){

		Slot * cur = bh->getplugins[a];

		while (cur != 0)
		{
			if (cur->value && cur->value->kdbGet)
			{
				if (cur->value->kdbGet (handle, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
				{
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (a == GET_RESOLVER || a == GET_STORAGE)
				{
					cur = 0;
				}
				else
				{
					cur = cur->next;
				}
			}
		}
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendSet (Plugin * handle, KeySet * ks, Key * parentKey)
{
	//TODO implement global plugins
	BackendHandle * bh = elektraPluginGetData (handle);
	if (!bh || !bh->setplugins)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "The backend handle is not defined correctly!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Slot * resolver = bh->setplugins[SET_RESOLVER];
	if (!resolver || !resolver->value)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "The resolver plugin is not defined!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!resolver->value->kdbSet)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "The set function of the resolver plugin is not defined!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int ret = resolver->value->kdbSet (handle, ks, parentKey);

	switch (ret)
	{
	case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	case ELEKTRA_PLUGIN_STATUS_ERROR:
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int status = ELEKTRA_PLUGIN_STATUS_SUCCESS;

	for (int a = SET_RESOLVER + 1; a < SET_COMMIT; a++)
	{
		if (bh->setplugins[a])
		{
			Slot * cur = bh->setplugins[a];
			while (cur != 0)
			{
				if (cur->value && cur->value->kdbSet)
				{
					if (cur->value->kdbSet (handle, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
					{
						ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Error while carrying out kdbSet before the commit: %s", keyName (bh->mountpoint));
						status = ELEKTRA_PLUGIN_STATUS_ERROR;
					}

				}
				cur = cur->next;
			}
		}
	}

	if (status == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		elektraBackendError (handle, ks, parentKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (elektraBackendCommit (handle, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		status = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	for (int a = SET_COMMIT + 1; a <= SET_POSTCOMMIT; a++)
	{
		if (bh->setplugins[a])
		{
			Slot * cur = bh->setplugins[a];
			while (cur != 0)
			{
				if (cur->value && cur->value->kdbSet)
				{
					if (cur->value->kdbSet (handle, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
					{
						status = ELEKTRA_PLUGIN_STATUS_ERROR;
						ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Error while carrying out kdbSet after the commit: %s", ks);
					}

				}
				cur = cur->next;
			}
		}
	}

	return status;
}

int elektraBackendCommit (Plugin * handle, KeySet * ks, Key * parentKey)
{
	BackendHandle * bh = elektraPluginGetData (handle);
	if (!bh || !bh->setplugins)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "The backend handle is not defined correctly!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (bh->setplugins[SET_COMMIT] && bh->setplugins[SET_COMMIT]->value && bh->setplugins[SET_COMMIT]->value->kdbCommit)
	{
		if (bh->setplugins[SET_COMMIT]->value->kdbCommit (handle, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Error during the commit phase: %s", ks);
		}

	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraBackendError (Plugin * handle, KeySet * ks, Key * parentKey)
{
	BackendHandle * bh = elektraPluginGetData (handle);
	if (!bh || !bh->errorplugins)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "The backend handle is not defined correctly!");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	for (int a = ERROR_PREROLLBACK; a <= ERROR_POSTROLLBACK; a++)
	{
		Slot * cur = bh->errorplugins[a];
		while (cur != 0)
		{
			if (cur->value && cur->value->kdbError)
			{
				if (cur->value->kdbError (handle, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
				{
					ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Error while carrying out kdbError(oh the irony): %s", ks);
				}
			}
			cur = cur->next;
		}
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("backend",
		ELEKTRA_PLUGIN_OPEN,	&elektraBackendOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraBackendClose,
		ELEKTRA_PLUGIN_GET,	&elektraBackendGet,
		ELEKTRA_PLUGIN_SET,	&elektraBackendSet,
		ELEKTRA_PLUGIN_COMMIT,	&elektraBackendCommit,
		ELEKTRA_PLUGIN_ERROR,   &elektraBackendError,
		ELEKTRA_PLUGIN_END);
}
