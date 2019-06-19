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

In order to use `JNI` in Elektra, the library `jni.h` must be provided for it. It is important to note, that Elektra uses SDK version 1.8, so you have to install this version of SDK and use while writing a Java plugin. This file is used later in `JNI Plugin`, which has to be enabled, while installing Elektra. More information on how to install `JNI Plugin` can be found [here](https://www.libelektra.org/plugins/jni). Please read carefully and follow instructions in `Installation` and `Enabling the Plugin` sections. If you don't have `jni.h` file, it can be easily downloaded and placed under this path:

- `/path/to/SDK/include`

or you can reinstall Java 1.8 SDK.

### JNA Binding

Java Native Access is Java technology (library), which like JNI allows a developer to run native code using only Java by providing access to native shared libraries. In order to use JNA binding, it should be firstly installed. More information you can find on the the GitHub [page](https://github.com/java-native-access/jna).

Generally speaking, `JNI` and `JNA` are both different java technologies that could be used to bind Java code and code written in C and C++. Even though, `JNI` gives the ability for developers to integrate Java code into C, what is not possible with `JNA`.

Here we will look into, how can we write Java Plugins using - `JNA Binding`.

### Writing a Plugin

Under the path: `/path/to/libelektra/src/bindings/jna/libelektra4j/src/main/java/org/libelektra` you can find the examples of already existing plugins and you can look into libelektra Java library, which is used for communication with the configuration database.

In order to write a new Java Plugin, the new class has to be created under the `plugin` folder. It has also to extend the `Plugin.java` interface, which contains all required methods to communicate with Elektra database. You can leave some of the methods not implemented, if there is no need in them.

The Plugin.java interface contains methods for the five basic functions of plugins:
- [`elektraPluginOpen`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga23c2eb3584e38a4d494eb8f91e5e3d8d),
- [`elektraPluginGet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gacb69f3441c6d84241b4362f958fbe313),
- [`elektraPluginSet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gae65781a1deb34efc79c8cb9d9174842c),
- [`elektraPluginError`](https://doc.libelektra.org/api/current/html/group__plugin.html#gad74b35f558ac7c3262f6069c5c47dc79), and
- [`elektraPluginClose`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga1236aefe5b2baf8b7bf636ba5aa9ea29)

It is expected that each method returns -1 if an error occured or 0 if everything worked as expected.

### Usage of Plugin

To use the bindings in a java project, we have to include the jar file `libelektra-version.jar` in the project. The version number is the same one as used for Elektra. This jar is created upon build, if you enable the jna bindings. You can also use maven to take care about the dependencies. After that you can take your implemented class out of `libelektra` folder and integrate it into your project.
