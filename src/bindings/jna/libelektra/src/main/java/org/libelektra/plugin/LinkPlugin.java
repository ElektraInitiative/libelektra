package org.libelektra.plugin;

import static org.libelektra.Elektra.KeyNewArgumentFlags.KEY_END;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;
import org.libelektra.exception.KDBException;

/**
 * This class can be used to validate link via the "check/link" meta key.
 * Initially it uses a {@link URLConnection} to check if the link is reachable, if not it also uses
 * a regex to check validity of the link.
 * */
public class LinkPlugin implements Plugin
{

	// OWASP url validation regex from here
	// https://owasp.org/www-community/OWASP_Validation_Regex_Repository
	private static final Pattern URL_REGEX = Pattern.compile (
		"^((((https?|ftps?|gopher|telnet|nntp)://)|(mailto:|news:))(%[0-9A-Fa-f]{2}|[-()_.!~*';/?:@&=+$,A-Za-z0-9])+)([).!';/?:,][[:blank:]])?$");

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
			ks.append (Key.create (root + "/infos/provides", "check/link"));
			ks.append (Key.create (root + "/infos/placements", "getstorage setstorage"));
			ks.append (Key.create (root + "/infos/author", "@aaronabebe"));
			final Key k = ks.lookup (root + "/infos/description");
			if (!k.isNull ())
			{
				// append to description
				k.setString (k.getString () + "Check if a link is reachable or is a valid http link");
			}
			return 0;
		}
		return 0;
	}

	@Override public int set (KeySet ks, Key parentKey) throws KDBException
	{
		for (final Key key : ks)
		{
			Key meta = key.getMeta ("check/link");
			if (meta == null || meta.isNull ())
			{
				return 0;
			}

			String keyValue = key.getString ();
			if (isReachable (keyValue))
			{
				return 1;
			}
			else
			{
				if (isValidHTTPLink (keyValue))
				{
					return 1;
				}
				parentKey.setError ("Link is not reachable and invalid");
				return -1;
			}
		}
		return 0;
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

	boolean isReachable (String link)
	{
		try
		{
			final URL url = new URL (link);
			final URLConnection conn = url.openConnection ();
			conn.setConnectTimeout (100);
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
		Matcher matcher = URL_REGEX.matcher (link);
		return matcher.matches ();
	}
}
