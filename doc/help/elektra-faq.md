# Frequently Asked Questions

## I am stuck. Where can I get help?

If this FAQ does not contain your question, [please open an issue](https://git.libelektra.org/issues).
You can simply remove all template text and it is enough if the issue only contains your question.
If you want you can [label it as question](https://git.libelektra.org/issues/labels/question), but we
can also categorize it for you.

Please do not waste too much time to find something out yourself. Information where people get
stuck is valuable to improve Elektra and its documentation. Even if you find out directly after
you posted the question: the pointer can be helpful for other people having the same problem.

## Isn't it easier to implement a new parser than to use Elektra?

No, it is not. And even if it were, the story does not end with implementing
a configuration file parser but, at least, you also need:

- operating-system-specific code to locate configuration files
- tools to change the configuration files
- validation to make such changes user friendly

Every successful project has implemented many features Elektra has.
But Elektra has the distinctive advantage that you can pick the features
as you need them. Not used plugins do not cause any overhead or dependency.
If you need new plugins or bindings, there is a community which can help you.
Furthermore, Elektra has a defined API and you can swap implementations as needed.

So it pays off to use Elektra -- in the short and in the long term.

## Why do I need Elektra if I already use configuration management tools?

Short answer: Try [puppet-libelektra](https://puppet.libelektra.org)
to see how useful it can be.

Longer answer:

Elektra abstracts [configuration settings](elektra-glossary.md), something
desperately needed within configuration management. Instead of rewriting
complete configuration files, which might create security problems,
Elektra operates precisely on the configuration setting you want to
change: leaving others as chosen by the application or distribution.
Furthermore, Elektra also allows us to _specify_ configuration settings,
which again brings benefits for configuration management tools.

Elektra is a radical step needed towards better configuration management:
Let us fix how applications access configuration settings, so that we
can properly access them, for example, from configuration management tools.

As an intermediate step, we can [mount](elektra-mounting.md)
existing configuration files and operate on them.

## Do we retain the old way of configuring things, i.e. manually editing an ini file in /etc?

Absolutely, you can think of libelektra as a small library in C that
reads a configuration file and returns a data structure if you do not
use any of its advanced features.

## Do we retain the old way reloading/restarting the system service?

Elektra does not interfere with restarting. It is a passive library.
It provides some techniques for reloading but they are optional (but we
recommend that you keep the in-memory and persistent configuration in
sync via notification).

## Is this an actual problem of Elektra or is it just me?

In case of doubt [please open an issue](https://git.libelektra.org/issues).
If the question was already answered or is already in the documentation, we will
simply point it out to you.

So do not worry too much, do not hesitate to ask any question. We welcome
feedback, only then we can improve the documentation such as this FAQ!

## What should I do if I found a bug?

Please check the [issue tracker](https://git.libelektra.org/issues) if it has already been reported.
If it has not, please [fill out the template](https://git.libelektra.org/issues/new).
If you are in doubt, please report it.

## How can I contribute to Elektra?

Due to the modular architecture we can accept nearly all contributions as plugins.
Please only make sure that the README.md clearly states the purpose and quality
of the plugin. If you need, e.g., a plugin that crashes the process make sure that
you tag (`infos/status`) it with `discouraged`.

Please start by reading [here](/.github/CONTRIBUTING.md).

## What is the Elektra’s license?

[New BSD license](/LICENSE.md) which allows us to have plugins link against GPL
and GPL-incompatible libraries. If you compile Elektra, e.g., with GPL plugins, the
result is GPL.

## Who are the authors?

[List of authors](/doc/AUTHORS.md).

## How can I advertise Elektra?

If questions about configuration come up, point users to https://www.libelektra.org
Display the SVG logos found at https://master.libelektra.org/doc/images/logo
and already rastered logos at https://github.com/ElektraInitiative/blobs/tree/master/images/logos
Distribute the flyer found at https://github.com/ElektraInitiative/blobs/raw/master/flyers/flyer.odt
And of course: talk about it!
