# Frequently Asked Questions

## I Am Stuck. Where Can I Get Help?

[Please open an issue](https://git.libelektra.org/issues).
You can simply remove all template text and it is enough if the issue only contains your question.
If you want you can [label it as question](https://git.libelektra.org/issues/labels/question), but we
can also categorize it for you.

Please do not waste too much time to find something out yourself. Information where people get
stuck is valuable to improve Elektra and its documentation. Even if you find out directly after
you posted the question: the pointer can be helpful for other people having the same problem.

## Isn't it Easier to Implement a new Parser Than to use Elektra?

No, it is not. And even if it were, the story does not end with implementing
a configuration file parser but, at least, you also need:

- operating-system-specific code to locate configuration files
- tools to change the configuration files
- validation to make such changes user-friendly

Every successful project has implemented many features Elektra has.
But Elektra has the distinctive advantage that you can pick the features
as you need them. Not used plugins do not cause any overhead or dependency.
If you need new plugins or bindings, there is a community which can help you.
Furthermore, Elektra has a defined API and you can swap implementations as needed.

So it pays off to use Elektra -- in the short and in the long term.

## Why Do I Need Elektra If I Already Use Configuration Management Tools?

Short answer: Try

- [ansible-libelektra](https://galaxy.ansible.com/elektra_initiative/libelektra)
- [chef-libelektra](https://supermarket.chef.io/cookbooks/kdb)
- [puppet-libelektra](https://puppet.libelektra.org)

to see how useful it can be.

Longer answer:

Elektra abstracts [configuration settings](elektra-glossary.md), something desperately needed within configuration management.
Instead of rewriting complete configuration files, which might create security problems due to ignoring distributions configuration files;
Elektra operates precisely on the configuration setting you want to change:
leaving others as chosen by the application or distribution.
Furthermore, Elektra also allows us to _specify_ configuration settings, which again brings benefits for configuration management tools.

Elektra is a radical step needed towards better configuration management:
Let us fix how applications access configuration settings, so that we can properly access them, for example, from configuration management tools.

See [our vision](/doc/VISION.md) for an example.

## If Elektra Already Exists so Long, why isn't it more Widespread?

There are two main reasons:

1. Research:
   First we needed to [explore the design space](/doc/dev/history.md).
   At that time, Elektra provided little benefit, except for niche applications.
   Then it was challenging to actually implement the [vision](/doc/VISION.md).
2. Bootstrapping:
   Developers would like to have everything smooth and shiny, like packages available as are part of their distributions.
   But Elektra only gets packaged, if there are already application using Elektra.
   We solved this problem, by [many intermediate steps](/doc/dev/history.md), e.g., [mounting](elektra-mounting.md) existing configuration files and operate on them.

## Do We Retain the Old Way of Configuring Things, i.e. Manually Editing an INI File in /etc?

Absolutely, you can think of libelektra as a small library in C that
reads a configuration file and returns a data structure if you do not
use any of its advanced features.

In fact, from the view of system-calls, a properly written configuration
parser within your application would do exactly the same as Elektra does:

```
stat("/etc/kdb/elektra.ecf", {st_mode=S_IFREG|0644, st_size=1996, ...}) = 0
open("/etc/kdb/elektra.ecf", O_RDONLY)  = 3
read(3, "..., 8191) = 1996
close(3)                                = 0
```

Writing configuration files is much more tricky, as Elektra avoids
data loss in the case of concurrent writes, even if the other
application does not use Elektra. Elektra uses optimistic writes
and rolls backs when it detects that configuration files were
modified.

## Do We Retain the Way of Reloading/Restarting the System Service?

Elektra does not interfere with restarting. It is a passive library.
It provides some techniques for reloading but they are optional. We
recommend, however, that you keep the in-memory and persistent
configuration always in sync via immediate writes on changes and
immediate reloading after notifications.

## Is This an Actual Problem of Elektra or Is It Just Me?

In case of doubt [please open an issue](https://git.libelektra.org/issues).
If the question was already answered or is already in the documentation, we will
simply point it out to you.

So do not worry too much, do not hesitate to ask any question. We welcome
feedback, only then we can improve the documentation such as this FAQ!

## What Should I Do If I Found a Bug?

Please check the [issue tracker](https://git.libelektra.org/issues) if it has already been reported.
If it has not, please [fill out the template](https://git.libelektra.org/issues/new).
If you are in doubt, please report it.

## How Can I Contribute to Elektra?

Due to the modular architecture we can accept many contributions as plugins.
Please only make sure that the README.md clearly states the purpose and quality
of the plugin.

Please start by reading [here](/.github/CONTRIBUTING.md).

## What Is Elektra’s License?

[New BSD license](/LICENSE.md) which allows us to have plugins link against GPL
and GPL-incompatible libraries. If you compile Elektra, e.g., with GPL plugins, the
result is GPL. We are [reuse](https://reuse.software/) compliant.

## Which version should I use?

If you already use 0.8, you might want to continue using it until 1.0 is released.
As announced [here](/doc/news/2019-08-06_0.9.0.md), the 0.9 series introduces incompatible
changes as needed, in particular cleanup for the 1.0 release is done.

For details of versioning principles, see [here](/doc/VERSION.md).

## Who Are the Authors?

[List of authors](/doc/AUTHORS.md).

## How Can I Advertise Elektra?

- If questions about configuration come up, point users to https://www.libelektra.org
- Display the SVG logos found at https://master.libelektra.org/doc/images/logo
- And already rastered logos at https://github.com/ElektraInitiative/blobs/tree/master/images/logos
- Distribute the flyer found at https://github.com/ElektraInitiative/blobs/raw/master/flyers/flyer.odt
- And of course: talk about it!
