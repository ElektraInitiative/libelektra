# History

## Begin

The development of Elektra started in 2004.
Initially the initiative only aimed at the straightforward idea to unify a configuration access API and a configuration format.
Elektra started by introducing an API with a few language bindings.

The idea of introducing a configuration access API was not new:
Most proprietary software systems already had similar APIs for a long time.
Nevertheless, it was clear that there was no portable configuration library available to be used for typical FLOSS applications.
For example, the configuration libraries X/Q/GSettings, KConfig, dconf, plist, and Windows Registry are tied to their respective platforms.

## Elektrify

The first idea to improve adoption of Elektra was that the Elektra Initiative itself patches applications to use Elektra.
This was done, e.g., for the X Server.
The maintainers of the X Server, however, had other plans and later improved their software to auto-detect hardware.
This was admittedly the right choice:
Elektra would not have been able to help with this at that time.

## Backends

It became obvious that not a single configuration format would suffice for everyone.
In a first step, this was achieved by introducing different backends, chosen by users at run-time.

## Mounting

An implementation providing only a single backend for the whole system proved to be too limited:
Individual applications cannot customize their backends for their own needs.
We implemented a layer similar to a virtual file system, which enables applications and system administrators to mount configuration files.

## Dependencies

Furthermore, dependences in the core made Elektra unattractive.
To solve this problem, we modularized Elektra so that users can select exactly the features they need.

Such changes made part of the software more complicated.
At first, we provided too little documentation for newcomers to grasp the abstraction mechanisms.
Then we started to put efforts into rebuilding the community by overhauling documentation, introducing more regression tests, writing tutorials, and designing a new website.
We succeeded by other FLOSS initiatives willing to use Elektra, and Elektra being packaged for many distributions.
