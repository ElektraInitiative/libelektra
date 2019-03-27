# How-To: Write a Java Plugin

This file serves as a tutorial on how to get started with writing a java plugin.

## Basics

If you want to know more about plugins in general, please check out the [How-To: Write a Plugin](plugins.md) page.

If you need a tutorial for java, please check out the [How-To: Java kdb](java-kdb.md) page.


#### Java Plugins

In order to write a new Java Plugin, the new class has to extend the Plugin.java interface. Still, not all the methods of the interface have to be implemented, if they are not required.

The Plugin.java interface contains methods for the five basic functions of plugins:
- [`elektraPluginOpen`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga23c2eb3584e38a4d494eb8f91e5e3d8d),
- [`elektraPluginGet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gacb69f3441c6d84241b4362f958fbe313),
- [`elektraPluginSet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gae65781a1deb34efc79c8cb9d9174842c),
- [`elektraPluginError`](https://doc.libelektra.org/api/current/html/group__plugin.html#gad74b35f558ac7c3262f6069c5c47dc79), and
- [`elektraPluginClose`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga1236aefe5b2baf8b7bf636ba5aa9ea29)

It is expected that each method returns -1 if an error occured or 0 if everything worked as expected.

An example of Java Plugin is the PropertiesStorage plugin which can be found in the PropertiesStorage.java file.