# About Elektra's Java Binding

Whenever a software initiative opens itself up by providing native support for another ecosystem, there are many aspects to consider.
Obviously, proper implementation in the target ecosystem's programming language is essential, but there are other aspects to consider, such as adhering to the ecosystem's established dependency management system, and providing documentation in a format that developers will expect.

In order to translate the existing Elektra's C API well, some key design decisions had to be made.
In particular, we had to address the following:

- Proper object-oriented representation of Elektra's entities

- Correct error propagation

- Transparent handling of native resources

- Integration into Elektra's build and Continous Integration (CI) pipelines - including the following key aspects:

  - Automated tests

  - Publish release artefacts to established dependency repositories

  - Installation of Java dependencies together with Elektra

- Documentation of the Java API without replicating the documentation of the underlying C API

To strike a balance between compatibility and the use of language features, Java 11 has been set as the target for compatibility.

## Java API

The Java binding must bridge the conceptual and technical gap between the native Elektra API and provide an experience that transparently hides native resource management as well as conceptual differences between the C and Java paradigms.

### Modelling Elektra's Entities for a Java Audience

Elektra's main entities are the key database, keysets and keys.
As simple as this sounds, there are some aspects of these underlying concepts that can benefit from leveraging the particular concepts of Java's object-oriented representation.

The following design objectives had been identified and implemented:

- API functions that only return read-only keys must be modelled to provide no write functionality to the underlying native key resources.
  This primarily affects meta-keys, which by definition are read-only and cannot contain binary data.

- Key and key set representations must leverage common Java interfaces.
  To facilitate this, `Key` implements `Iterable<ReadableKey>` for accessing its meta-keys while `KeySet` extends `AbstractSet<Key>` and implements `NavigableSet<Key>`.

  ![Overview `KeySet` and `Key` type hierarchy](key-type-hierarchy.png)

- Access to Elektra's key database needs to implement the convenient interface `AutoClosable` to further enhance the transparency of the underlying native resource.

- Common modern design paradigms like fluent interfaces, support of `Optional` and `Stream` interoperability are provided to enable seamless integration of existing state-of-the art Java code.

### Error Propagation

Elektra's C API uses two mechanisms to communicate warning and error information.
One is by populating a key object provided by the caller of a function and the other is by specifically defined return values.

To accommodate these two mechanics in the Java binding, the following main strategies are employed:

- `Optional` return values are used instead of throwing an error whenever a function might not return a requested object.

- An exception hierarchy is modelled to represent Elektra's error codes.
  The binding automatically translates error information provided by Elektra's C API into appropriate exceptions.

  A particular problem we encountered when implementing this feature was that initially we did not immediately extract all available error information from the respective error keys provided by Elektra when creating an exception object. This led to native resources that had already been deallocated being accessed when an exception was later evaluated.

  ![`Exception` hierarchy](exception-hierarchy.png)

  ![`RuntimeException` hierarchy](runtime-exception-hierarchy.png)

## Native Reference Handling

In C, memory allocation is done manually using functions such as `malloc` and `free`, while in Java memory management is done automatically by garbage collection.
C allows direct manipulation of pointers, while Java uses references without direct access to memory addresses.
C is prone to memory-related errors such as leaks and invalid memory accesses, while Java's automatic memory management reduces such risks.
Overall, C offers more control over memory, but requires manual memory management, while Java abstracts away the associated complexities.

To bridge the gap between Java's automatic memory recovery mechanisms and C's explicit way of managing memory, reference counters and cleaner implementations must be used to enable the Java Virtual Machine (JVM) garbage collector to properly clean up native resources.

In order for native resource allocation to work correctly with bindings such as the Java binding, Elektra's support for key and key set reference counting had to be extended.
Whenever a native resource is represented by a non-native object, the resource's reference counter is incremented.
When the non-native representation object is destroyed, the counter is decremented and an attempt is made to delete the native resource.
Only if the reference count is zero will the native resource actually be deallocated.
In Elektra's Java binding, this is facilitated by a `Cleaner` implementation to allow the garbage collector to correctly clean up native resources.
Special care has been taken to correctly initialise Java representations of native resources when passed to a Java-based Elektra plugin.

Getting the reference counter implementation and the garbage collector interaction right proved to be a significant challenge.
This was especially true when Java plugin support was implemented.

There are a few important things to keep in mind when working with native resources:

- Reference counting support from the C API is required.

- Never depend on when the garbage collector is called.

- Never rely on native resources in exceptions, as their evaluation may be out of scope.

## Plugin Support

Support for writing Elektra plugins in Java has been extended.
To avoid problems with the Java Native Interface (JNI) used by Elektra to communicate with Java plugins, and to allow multiple Java plugins to be used simultaneously, Elektra's `process' plugin had to be modified.

## Build Tooling

Elektra's build tooling is based on CMake, so building and publishing Java assets is not a native feature.
Some preliminary work had already been done to create a Java binding based on the Maven build tooling.
A flexible and future-proof build setup for Java-related assets was one of the first decisions to be made.

The following criteria have been established for any candidate to be seriously considered:

- Existing dependencies must be supported by the chosen build system.

- An established system with a supportive community would be a great advantage.

- Build performance should not be adversely affected.

- Integration with existing build infrastructure is no more complicated than before.

- The benefits of the transition outweigh the potential costs, making it a worthwhile investment.

- Ease of use, community support and flexibility were the most important factors to consider.

- The fact that Maven does not provide native support for Kotlin, which is becoming increasingly popular with developers in the Java Virtual Machine (JVM) environment, was also an important factor to consider.

- Either we have the necessary expertise and resources to make a successful transition or the new build system should be easy and intuitive for the development team to use.

After considering the obvious candidates (Ant, CMake, Maven and Gradle), we confidently chose Gradle as the build tool for the Java binding, paving the way for a future Kotlin binding.

To briefly address the eliminated candidates:

- Ant is a popular build tool that uses XML files to define build processes.
  It is flexible and can be used for a variety of languages, but it can be more verbose and less user-friendly than other build tools.

- CMake is a build tool with limited support for Java.
  It is popular for C/C++ projects and can be extensively customised, but it can be more difficult to use than other build tools and is not as flexible for multi-language projects.
  As Elektra is a C-based software initiative, this system would have had the advantage of being already in use.

- Due to its declarative nature, Maven is not particularly flexible.
  However, it has been the de-facto standard for Java projects for a long time, and caters to the standard dependency management system used in the Java world.

The main reasons for this decision were:

- Gradle is the modern alternative to Maven, using the same dependency management infrastructure.

- It also opens up new opportunities for the growth of Elektra's Java bindings and other Java-based language bindings.

### CI Integration and Release Asset Publishing

Dependencies for Gradle and Java had to be included in the plethora of different CI test build environments.
Some exceptions to testing for unsupported features for some Mac OS builds (e.g. `gopts`) had to be made, making the build scripts a little more complicated.

Publishing assets to the sonatype repository is done using Gradle's `maven-publish` and `signing` plugins.
