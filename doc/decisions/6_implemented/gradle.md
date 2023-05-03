# Replacing Maven as Java-related build system with Gradle

## Problem

Originally, Maven was used as the build system for Elektra's Java-related modules.
When it came time to modernize and improve Elekta's Java bindings, the question of which build system to use had to be answered.

## Constraints

- Existing dependencies must be supported by the chosen build system.
- An established system with a supportive community would be of great advantage.
- Build performance should not be negatively impacted.
- Integration with existing build infrastructure is no more complicated than before.
- The benefits of the transition outweigh the potential costs, making it a worthwhile investment.
- Ease of use, community support, and flexibility were the most important factors to consider.
- The fact that Maven does not provide native support for Kotlin, which is becoming increasingly popular with developers, was also an important factor to consider.

## Assumptions

The development team either has the necessary expertise and resources for a successful transition, including knowledge of the new build system and familiarity with its syntax and conventions, or the new build system should be easy and intuitive for the development team to use, especially if the team is not already familiar with the new system.

## Considered Alternatives

- Ant is a popular build tool that uses XML files to define build processes.
  It is flexible and can be used for a variety of languages, but it can be more verbose and less user-friendly than other build tools.

- CMake is a build tool, which only has limited support for Java.
  It is popular for C/C++ projects and can be extensively customized, but it can be more difficult to use than other build tools and is not as flexible for projects that use multiple languages.
  Since Elektra is a C-based software project, this system would have had the advantage of already being in use.

## Decision

We are switching from Maven to Gradle.

- Gradle is more flexible and customizable than Maven.
  It uses a Groovy-based DSL that allows developers to write custom plugins and scripts easily, making it easier to configure and automate complex build processes.
- Gradle is faster than Maven.
  It offers more efficient incremental builds, meaning it can quickly determine which parts of the codebase have changed and only rebuild those parts.
  This might not yet bring a great improvement but allows for future scalability.
- Gradle's dependency management system is more flexible and offers more advanced features than Maven, including the ability to manage transitive dependencies and exclude specific unwanted dependencies.
  It also supports the same dependency eco-system as Maven does.
- Gradle natively supports Kotlin.
- Gradle offers an easy migration path from Maven.

## Rationale

Gradle is not only the modern alternative to Maven, using the same dependency management infrastructure, but also opens up new possibilities for the growth of Elektra's Java bindings and other Java-based language bindings.

## Implications

- Changed the setup for building Java bindings to use Gradle instead of Maven.
- The build images needed to be modified to create the necessary dependencies for integrating the Java bindings using Gradle instead of Maven.
- Assets created will be transferred to Sonatype's repository so developers can use the dependency framework used by Gradle (and Maven).
- Developers need to switch to using Gradle.
- Leverage Gradle's caching capabilities to improve the performance of CI (continuous integration) builds.
  Gradle has several levels of cache.
  The global cache, which is shared by all projects built with Gradle on a given machine, stores artifacts such as JAR files and metadata downloaded from remote repositories and is used automatically.
  Using the local build cache is not beneficial for CI tasks.
  Depending on the actual build performance and whether it needs to be improved, configuring the remote build cache may be useful.
  It can be used to store the output of specific tasks or the entire build cache.
  Remote build caching can help speed up builds by reusing artifacts from previous builds, especially when building large projects with many dependencies - which is not currently the case with Elektra.
