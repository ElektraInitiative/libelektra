# Website Release

- guid: 102b84a3-c41e-485c-8fe2-f12a24b3fbfd
- author: Marvin Mall
- pubDate: Thu, 22 Dec 2016 17:46:19 +0100
- shortDesc: introduces new Elektra website with snippet sharing

## Highlight

1. Release of new Elektra website with an integrated service for
   sharing of configuration snippets.
2. The website also supports conversion between different
   configuration formats.
3. Website structures documentation and news sections in a new way.

## Introduction

With Elektra developing into a more and more reliable as well as
popular system to manage system configurations, the demand for a
better public appearance increases as well. For this reason, we
are happy to be able to announce the release of our new
[website](https://www.libelektra.org)!

The new website does not only give us a chance to better present
ourselves to the open world, it also enables us to structure our
project documentation better. We hope that this will make it easier
for our users to get started with Elektra and all of its awesome
features!

Besides the documentation, the website does also include a database
that can be used to share, search, download and convert configuration
snippets in various formats. We hope that this tool helps developers
and administrators, but also anyone else to simplify their
configuration processes when they have to look for a specific
configuration snippet. Btw. with snippet we mean that you can
also share parts of configuration files that you find particular
useful!

But sharing of snippets does not only help other users, it can help
yourself as well because you can search for them easier. You also
have access to the snippets in various formats at any time, allowing
you to use them across multiple system by mounting them with the
[curlget](https://master.libelektra.org/src/plugins/curlget) resolver!

## The Website

The website was written by Marvin Mall in the course of his
[bachelor thesis](https://www.libelektra.org/ftp/elektra/publications/mall2016rest.pdf)
as part of the frontend he developed for his snippet sharing service.
His main goals were to create a proper appearance for Elektra, but
also to create a platform that promotes his service. We think that
this worked out quite well by connecting the website with the service
the way it was done.

### Documentation

An important aspect of the new website was to make existing documentation
more transparent and structured. A lot of documentation files have been
changed to achieve this goal and an equal amount of effort was put into
writing a system that decouples the documentation structure on the
website from the structure used within the Elektra repository.

The tutorials section was partially reworked to make the first steps
together with Elektra easier for our users. Clearly the effort put into
the tutorials is worth it. Thanks to Erik Schnetter for the valuable
feedback where improvements are needed and Christoph Weber for (re)writing
the tutorials!

We should note -- as always in software -- that the structure on the website
is not final yet and will definitely develop over time, especially the
bindings and libraries sections will get some more attention.

If you are interested in the techniques we use to structure our files,
you can have a look at the
[website readme](/src/tools/website/README.md).
The website is already the fourth view of our Markdown pages!
The others are man pages, doxygen, and github.

### Homepage & News

Besides the documentation we also wanted a place to properly present
ourselves and our news around Elektra. For this reason we created a new
home page which shall give an overview of what Elektra is and can do.
Additionally to that, we also added a news section to keep you better
up-to-date!

We hope that you enjoy our new appearance as much as we do!

### Snippet Sharing

Another important part of the website and also without doubt the part
that took most effort to create, is the service that allows for sharing
of configuration snippets. It is run by a REST service fully built with
the help of [CppCMS](http://cppcms.com/) on basis of Elektra as
data store. All data concerning snippets and user accounts is stored
in Elektra’s key database (of course with password being properly hashed).

The service allows you to paste configuration snippets in various (supported)
formats and to tag, describe and name them. This in return allows you to
search snippets by keywords and to download them -- even in other formats than
the format used for uploading.

Clearly the service is meant to be driven by its users. Therefore we ask
you to share your own configuration snippets, maybe they can be of help, e.g.,
be a time saver for someone else!

Snippets shared with the service are
[BSD licensed](https://www.libelektra.org/devgettingstarted/license).
The snippets can also be downloaded directly as bundle from a separate
[GitHub repository](https://github.com/ElektraInitiative/snippets).
As soon as a snippet is added, changed or deleted on the website, a job
that updates the repository is triggered. So you can expect the repository
to be always up-to-date.

### NoScript

The website is fully written with the help of AngularJS and is therefore
heavily based on JavaScript. This should be no issue though as the
website does only use resources that can be found in the official Elektra
repository:

1. So in case you cannot or do not want to use JavaScript, you
   can find all resources also [here](https://git.libelektra.org).
2. If you are only worried about executed untrusted JavaScript,
   you can study and improve the
   [Web Frontend](/src/tools/website/README.md),
   which builds the website.
   Based on this, we hope you disable `NoScript` for our page
   so that you are able to share snippets!

## Domains

All Elektra Domains directly hosted by us are now only served by `https`.
The former `http` sites are only redirects to `https`. This might cause
trouble with some software, e.g., update `/etc/apt/sources.list`:

```
deb     [trusted=yes] https://build.libelektra.org/debian/ wheezy main
deb-src [trusted=yes] https://build.libelektra.org/debian/ wheezy main
```

The build Server is no longer reachable at port 8080, but now only directly at
[https://build.libelektra.org/](https://build.libelektra.org/).

While most `libelektra.org` now point to the new website, you can still
directly go to [GitHub](https://git.libelektra.org) and also to the
[bug tracker](https://bugs.libelektra.org).

The old Wordpress installation was shut down because of security concerns.

## Feedback

At this point there is not much more to say about the new website except for:
Feel free to explore it!

We greatly appreciate all feedback, be it for the website, the snippet sharing
service or other parts of the Elektra Initiative. We always have an open ear
for suggestions and we also like to help with technical issues, simply
[leave us a note on GitHub](https://bugs.libelektra.org)!

## Stay tuned!

Subscribe to the reimplemented
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

For any questions and comments, please contact the
[mailing list](https://lists.sourceforge.net/lists/listinfo/registry-list),
use the issue tracker [on GitHub](https://bugs.libelektra.org)
or write an email to elektra@markus-raab.org.
For issues or feedback concerning the website, you can also
contact us at website@libelektra.org.

[Permalink to this NEWS entry](https://www.libelektra.org/news/website-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
Marvin & Markus
