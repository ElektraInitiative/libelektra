package elektra.plugin;

import elektra.*;
import java.io.FileInputStream;
import java.io.BufferedInputStream;
import java.io.FileOutputStream;
import java.io.BufferedOutputStream;
import java.util.Properties;
import java.util.Map;

public class PropertiesStorage implements Plugin {
	public PropertiesStorage() {
	}

	public int open(KeySet conf, Key errorKey) {
		return 0;
	}

	public int get(KeySet ks, Key parentKey) {
		String root="system/elektra/modules/jni";
		if (parentKey.isBelowOrSame(Key.create(root, Key.KEY_END))) {
			ks.append(Key.create(root+"/infos/provides", Key.KEY_VALUE, "storage", Key.KEY_END));
			ks.append(Key.create(root+"/infos/placements", Key.KEY_VALUE, "getstorage setstorage", Key.KEY_END));
			Key k = ks.lookup(root+"/infos/description");
			if (!k.isNull()) {
				// append to description
				k.setString(k.getString() +
					"Get + set properties files");
			}
			return 0;
		}
		Properties properties = new Properties();
		try (BufferedInputStream stream =
			new BufferedInputStream(new FileInputStream(parentKey.getString()))) {
			properties.load(stream);
		} catch (Exception e) {
			parentKey.setError("Could not read file");
			return -1;
		}
		for (Map.Entry<Object, Object> e:properties.entrySet()) {
			System.out.println(e.getKey() + " " + e.getValue());
			ks.append(Key.create(parentKey.getName()+"/"+e.getKey(),
					Key.KEY_VALUE, e.getValue(),
					Key.KEY_END));
		}
		for (int i=0; i< 100; i++) {
			parentKey.addWarning("blah %4d", i);
		}
		parentKey.setError("blah %4d", 20);
		return -1;
	}

	public int set(KeySet ks, Key parentKey) {
		Properties properties = new Properties();
		for (Key k: ks) {
			String newName = k.getName().substring(parentKey.getNameSize());
			properties.setProperty(newName, k.getString());
		}
		try (BufferedOutputStream stream =
			new BufferedOutputStream(new FileOutputStream(parentKey.getString()))) {
			properties.store(stream, "written by elektra using Java Properties");
		} catch (Exception e) {
			parentKey.setError("Could not write file");
			return -1;
		}
		return 0;
	}

	public int error(KeySet ks, Key parentKey) {
		return 0;
	}

	public int close(Key parentKey) {
		return 0;
	}

}
