# How-To: Write a Java Plugin

This file serves as a tutorial on how to get started with writing a java plugin.

## Basics

If you want to know more about plugins in general, please check out the [How-To: Write a Plugin](/doc/tutorials/plugins.md) page.
If you need a tutorial for java, please check out the [How-To: Java kdb](/doc/tutorials/java-kdb.md) page.

## Two Types of Java Plugins

Before we will take a look into how to write a plugin in Java, it is important to note, that there are two ways of doing that:

- using JNI plugin
- using JNA binding

If you would like to write a plugin in Java and look into the documentation, then you will definitely meet two notions like `JNI Plugin` and `JNA Binding`, so let's clarify, what they are.

### JNI Plugin

Java Native Interface is a framework, which allows Java code to execute or to be executed by programms, written in other languages like C, C++ and Assembly. Most of Elektra’s plugins are written in C and C++. Developer can use the JNI plugin, which was created specifically for Elektra to write plugins using Java code. For more information on JNI plugins, please take a look [here](/src/plugins/jni/README.md).

In order to use `jni` in Elektra, the library `jni.h` must be provided for it. This file is used by the JNI Plugin, which has to be enabled, while installing Elektra. More information on how to install the JNI plugin can be found [here](/src/plugins/jni/README.md). Please read carefully and follow instructions in the “Installation” and “Enabling the Plugin” sections. If you don't have the `jni.h` file, it can be easily downloaded and placed under this path:

- `/path/to/SDK/include`

or you can reinstall the Java SDK.

### JNA Binding

Java Native Access is Java technology (library), which like JNI allows a developer to run native code using only Java by providing access to native shared libraries. In order to use the JNA binding, it should be installed first. You can find more information on [JNA’s GitHub page](https://github.com/java-native-access/jna).

Generally speaking, `JNI` and `JNA` are both different Java technologies that could be used to bind Java code and code written in C and C++. `JNI` gives developers the ability to integrate Java code into C too. That is not possible with `JNA`.

Here we will look into, how can we write Java Plugins using - `JNA Binding`.

### Writing a Plugin

Under the the following [path](/src/bindings/jna/libelektra5j/src/main/java/org/libelektra) you can find the examples of already existing plugins and you can look into libelektra Java library, which is used for communication with the configuration database.

In order to write a new Java Plugin, the new class has to be created under the `plugin` folder. It has also to extend the `Plugin.java` interface, which contains all required methods to communicate with Elektra’s database. You can leave some of the methods unimplemented, if there is no need for them.

The `Plugin.java` interface contains methods for the five basic functions of plugins:

- [`elektraPluginOpen`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga23c2eb3584e38a4d494eb8f91e5e3d8d),
- [`elektraPluginGet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gacb69f3441c6d84241b4362f958fbe313),
- [`elektraPluginSet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gae65781a1deb34efc79c8cb9d9174842c),
- [`elektraPluginError`](https://doc.libelektra.org/api/current/html/group__plugin.html#gad74b35f558ac7c3262f6069c5c47dc79), and
- [`elektraPluginClose`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga1236aefe5b2baf8b7bf636ba5aa9ea29)

It is expected that each method returns -1 if an error occurred or 0 if everything worked as expected.

### Usage of Plugin

To use the bindings in a Java project, we have to include the jar file `libelektra-version.jar` in the project. The version number is the same one as used for Elektra. This jar is created upon build, if you enable the JNA binding. You can also use Maven to take care of the dependencies. After that you can take your implemented class out of the `libelektra` folder and integrate it into your project.
