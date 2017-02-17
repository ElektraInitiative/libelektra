package org.libelektra.plugin;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Properties;

import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

public class PropertiesStorage implements Plugin {

	@Override
	public int open(final KeySet conf, final Key errorKey) {
		return 0;
	}

	@Override
	public int get(final KeySet ks, final Key parentKey) {
		final String root = "system/elektra/modules/jni";
		if (parentKey.isBelowOrSame(Key.create(root, Key.KEY_END))) {
			ks.append(Key.create(root + "/infos/provides", Key.KEY_VALUE, "storage", Key.KEY_END));
			ks.append(Key.create(root + "/infos/placements", Key.KEY_VALUE, "getstorage setstorage", Key.KEY_END));
			final Key k = ks.lookup(root + "/infos/description");
			if (!k.isNull()) {
				// append to description
				k.setString(k.getString() + "Get + set properties files");
			}
			return 0;
		}
		final Properties properties = new Properties();
		try (final BufferedInputStream stream = new BufferedInputStream(new FileInputStream(parentKey.getString()))) {
			properties.load(stream);
		} catch (final IOException e) {
			parentKey.setError("Could not read file");
			return -1;
		}
		for (final Map.Entry<Object, Object> e : properties.entrySet()) {
			ks.append(Key.create(parentKey.getName() + "/" + e.getKey(), Key.KEY_VALUE, e.getValue(), Key.KEY_END));
		}
		return 0;
	}

	@Override
	public int set(final KeySet ks, final Key parentKey) {
		final Properties properties = new Properties();
		for (final Key k : ks) {
			final String newName = k.getName().substring(parentKey.getNameSize());
			properties.setProperty(newName, k.getString());
		}
		try (final BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(parentKey.getString()))) {
			properties.store(stream, "written by elektra using Java Properties");
		} catch (final IOException e) {
			parentKey.setError("Could not write file");
			return -1;
		}
		return 0;
	}

	@Override
	public int error(final KeySet ks, final Key parentKey) {
		return 0;
	}

	@Override
	public int close(final Key parentKey) {
		return 0;
	}

}
