package org.libelektra.plugin;


import static org.libelektra.Key.CreateArgumentTag.KEY_END;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

/**
 * This class can be used to validate link via the "check/link" meta key.
 * Initially it uses a {@link URLConnection} to check if the link is reachable.
 * You can supply an optional timeout for the reachability check.
 * If no timeout or an empty timeout is provided, the plugins defaults to
 * checking the validity of the URL via {@link URL} and its {@link MalformedURLException}.
 *
 * Only checks the connection during `kdbSet()` operations.
 *
 */
public class LinkPlugin implements Plugin
{

	@Override public KeySet getConfig ()
	{
		return null;
	}

	@Override public int open (KeySet conf, Key errorKey)
	{
		return 0;
	}

	@Override public int get (KeySet ks, Key parentKey) throws KDBException
	{
		final String root = "system:/elektra/modules/jni";
		if (parentKey.isBelowOrSame (Key.create (root, KEY_END)))
		{
			ks.append (Key.create (root + "/infos/provides", "check"));
			ks.append (Key.create (root + "/infos/placements", "presetstorage"));
			ks.append (Key.create (root + "/infos/author", "@aaronabebe"));
			ks.append (Key.create (root + "/infos/metadata", "check/link"));
			ks.append (Key.create (root + "/infos/description", "Check if a link is reachable or is a valid link"));
			return 0;
		}
		return 0;
	}

	@Override public int set (KeySet ks, Key parentKey) throws KDBException
	{
		List<String> invalidKeys = new ArrayList<> (ks.size ());

		for (final Key key : ks)
		{
			Optional<Key> meta = key.getMeta ("check/link");
			if (meta.isEmpty ())
			{
				return 0;
			}

			String timeout = meta.get ().getString ();
			String keyValue = key.getString ();

			if (timeout.isEmpty ())
			{
				if (!isValidHTTPLink (keyValue))
				{
					invalidKeys.add (key.getName ());
				}
			}
			else
			{
				if (!isReachable (keyValue, Integer.parseInt (timeout)))
				{
					if (!isValidHTTPLink (keyValue))
					{
						invalidKeys.add (key.getName ());
					}
				}
			}
		}

		if (invalidKeys.isEmpty ())
		{
			return 1;
		}
		else
		{
			parentKey.setError ("Found invalid or unreachable links: " +
					    invalidKeys.stream ().collect (Collectors.joining (",")));
			return -1;
		}
	}

	@Override public int error (KeySet ks, Key parentKey)
	{
		return 0;
	}

	@Override public int close (Key parentKey)
	{
		return 0;
	}

	@Override public String getName ()
	{
		return null;
	}

	boolean isReachable (String link, int timeout)
	{
		try
		{
			final URL url = new URL (link);
			final URLConnection conn = url.openConnection ();
			conn.setConnectTimeout (timeout);
			conn.connect ();
			conn.getInputStream ().close ();
			return true;
		}
		catch (IOException e)
		{
			// URLConnection.connect() throws IOException if the host is not reachable
			return false;
		}
	}

	private boolean isValidHTTPLink (String link)
	{
		try
		{
			new URL (link);
			return true;
		}
		catch (MalformedURLException e)
		{
			// URLConnection.connect() throws IOException if the host is not reachable
			return false;
		}
	}
}
