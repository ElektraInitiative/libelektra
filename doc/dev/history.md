# History

## Elektrify

The development of Elektra started in 2004.
Initially the initiative only aimed at the straightforward idea to unify a configuration access API and a configuration format.
Elektra started by introducing an API with a few language bindings.

The idea of introducing a configuration access API was not new:
Most proprietary software systems already had similar APIs for a long time.
Nevertheless, it was clear that there was no portable configuration library available to be used for typical FLOSS applications.
For example, the configuration libraries X/Q/GSettings, KConfig, dconf, plist, and Windows Registry are tied to their respective platforms.

The first idea to improve adoption of Elektra was that the Elektra Initiative itself patches applications to use Elektra.
This was done, e.g., for the X Server.
The maintainers of the X Server, however, had other plans and later improved their software to auto-detect hardware.
This was admittedly the right choice for them:
Elektra would not have been able to help them at that time.

## Mounting

One obvious limitation of Elektra was that no gradual migration is possible and previous configuration files would need to be rewritten.
In a first step, we introduced backends, chosen by users at run-time.

An implementation providing only a single backend for the whole system proved to be too limited:
Individual applications cannot customize their backends for their own needs.
We implemented a layer similar to a virtual file system, which enables applications and system administrators to mount configuration files.

## Dependencies

Furthermore, dependences in the core made Elektra unattractive.
To solve this problem, we modularized Elektra so that users can select exactly the features they need.

Such changes made part of the software more complicated.
At first, we provided too little documentation for newcomers to grasp the abstraction mechanisms.
Then we started to put efforts into rebuilding the community by overhauling documentation, introducing more regression tests, writing tutorials, and designing a new website.

## Elektrify: Back Again

In the beginning of Elektra, there was no vision how configuration management and Elektra would work together.
After many years of research, we now have a [clear vision](/doc/VISION.md) how a world with Elektra would look like.

To actually improve the adoption of Elektra, however, configuration management cannot help directly.
Instead we need applications that depend on Elektra, so that Elektra gets packaged for all distributions.
We now elektrify KDE, GNOME and XFCE to achieve this goal.
Furthermore, we are now in the process of doing the last cleanups before version 1.0.
