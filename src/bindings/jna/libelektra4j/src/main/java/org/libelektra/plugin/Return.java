package org.libelektra.plugin;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class Return implements Plugin
{

	public static final String PLUGIN_NAME = "Return";

	@Override public KeySet getConfig ()
	{
		return KeySet.create ();
	}

	@Override public int open (final KeySet conf, final Key errorKey)
	{
		return 0;
	}

	@Override public int get (final KeySet ks, final Key parentKey)
	{
		return 10;
	}

	@Override public int set (final KeySet ks, final Key parentKey)
	{
		return 20;
	}

	@Override public int error (final KeySet ks, final Key parentKey)
	{
		return 30;
	}

	@Override public int close (final Key parentKey)
	{
		return 0;
	}

	@Override public String getName ()
	{
		return PLUGIN_NAME;
	}
}
