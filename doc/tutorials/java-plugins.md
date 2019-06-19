# How-To: Write a Java Plugin

This file serves as a tutorial on how to get started with writing a java plugin.

## Basics

If you want to know more about plugins in general, please check out the [How-To: Write a Plugin](plugins.md) page.
If you need a tutorial for java, please check out the [How-To: Java kdb](java-kdb.md) page.

## Two types of Java Plugins

Before we will take a look into how to write a plugin in Java, it is important to note, that there are two ways of doing that:

- using JNI plugin
- using JNA binding

If you would like to write a plugin in Java and look into the documentation, then you will definitely meet two notions like `JNI Plugin` and `JNA Binding`, so let's clarify, what they are.

### JNI Plugin

Java Native Interface is framework, which allows the Java code to execute or to be executed by the programms, written in other languages like C, C++ and Assembly. Since most of Elektra plugins are written in C and C++, the developer can use JNI plugin, which was created specifically for Elektra. This plugin allows developers to integrate Java into C code. More on JNI Plugins, you can read [here](https://www.libelektra.org/plugins/jni).

### JNA Binding

Java Native Access

#### Writing a Plugin

In order to write a new Java Plugin, the new class has to extend the Plugin.java interface. Still, not all the methods of the interface have to be implemented, if they are not required.

The Plugin.java interface contains methods for the five basic functions of plugins:
- [`elektraPluginOpen`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga23c2eb3584e38a4d494eb8f91e5e3d8d),
- [`elektraPluginGet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gacb69f3441c6d84241b4362f958fbe313),
- [`elektraPluginSet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gae65781a1deb34efc79c8cb9d9174842c),
- [`elektraPluginError`](https://doc.libelektra.org/api/current/html/group__plugin.html#gad74b35f558ac7c3262f6069c5c47dc79), and
- [`elektraPluginClose`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga1236aefe5b2baf8b7bf636ba5aa9ea29)

It is expected that each method returns -1 if an error occured or 0 if everything worked as expected.

An example of Java Plugin is the PropertiesStorage plugin which can be found in the PropertiesStorage.java file.
