package org.libelektra.plugin;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class PropertiesStorage implements Plugin
{

	public static final String PLUGIN_NAME = "PropertiesStorage";

	@Override public KeySet getConfig ()
	{
		return KeySet.create ();
	}

	@Override public int open (KeySet conf, Key errorKey)
	{
		return 0;
	}

	@Override public int get (KeySet keySet, Key parentKey)
	{
		var root = "system:/elektra/modules/jni";
		if (parentKey.isBelowOrSame (Key.create (root)))
		{
			keySet.append (Key.create (root + "/infos/provides", "storage"));
			keySet.append (Key.create (root + "/infos/placements", "getstorage setstorage"));
			Optional<Key> oDescriptionKey = keySet.lookup (root + "/infos/description");
			if (oDescriptionKey.isEmpty ())
			{
				return 0;
			}
			
			// append to description
			var descriptionKey = oDescriptionKey.get ();
			descriptionKey.setString (descriptionKey.getString () + "Get + set properties files");
		}
		
		var properties = new Properties ();
		try (var stream = new BufferedInputStream (new FileInputStream (parentKey.getString ())))
		{
			properties.load (stream);
		}
		catch (IOException e)
		{
			parentKey.setError ("Could not read file");
			return -1;
		}
		for (Map.Entry<Object, Object> e : properties.entrySet ())
		{
			keySet.append (Key.create (parentKey.getName () + "/" + e.getKey (), e.getValue ()));
		}
		return 0;
	}

	@Override public int set (KeySet keySet, Key parentKey)
	{
		var properties = new Properties ();
		for (var key : keySet)
		{
			properties.setProperty (key.getName ().substring (parentKey.getNameSize ()), key.getString ());
		}
		try (var stream = new BufferedOutputStream (new FileOutputStream (parentKey.getString ())))
		{
			properties.store (stream, "written by elektra using Java Properties");
		}
		catch (IOException e)
		{
			parentKey.setError ("Could not write file");
			return -1;
		}
		return 0;
	}

	@Override public int error (KeySet keySet, Key parentKey)
	{
		return 0;
	}

	@Override public int close (Key parentKey)
	{
		return 0;
	}

	@Override public String getName ()
	{
		return PLUGIN_NAME;
	}
}
