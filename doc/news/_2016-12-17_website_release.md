# Website Release #

- guid: 102b84a3-c41e-485c-8fe2-f12a24b3fbfd
- author: Marvin Mall
- pubDate: <TODO>
- shortDesc: introduces new Elektra website with snippet sharing

## Highlight ##

Release of new Elektra website with an integrated service for sharing
of configuration snippets. Also snippet conversion without sharing
is supported. Proper documentation and news sections were introduced
as well.

## Introduction ##

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
and administrators, but also normal users to simplify their
configuration processes when they have to look for a specific
configuration snippet.

But sharing of snippets does not only help other users, it can help
yourself as well because you can search for them easier. You also
have access to the snippets in various formats at any time, allowing
you to use them across multiple system by mounting them with the
[curlget](https://tree.libelektra.org/src/plugins/curlget) resolver!

## The Website ##

The website was written by Marvin Mall in the course of his bachelor thesis
as part of the front-end he developed for his snippet sharing service.
His main goals were to create a proper appearance for Elektra, but
also to create a platform that promotes his service. We think that
this worked out quite well by connecting the website with the service
the way it was done.

### Documentation ###

An important aspect of the new website was to make existing documentation
more transparent and structured. A lot of documentation files have been
changed to achieve this goal and an equal amount of effort was put into
writing a system that decouples the documentation structure on the
website from the structure used within the Elektra repository.

The tutorials section was partially reworked to make the first steps
together with Elektra easier for our users. Clearly the effort put into
the tutorials is worth it. Thanks to Christoph Weber for that!

We should note though that the structure on the website is not really
final yet and will definitely develop over time, especially the bindings
and libraries sections will get some more attention.

If you are interested in what techniques we use to structure our files,
you can have a look at the
[rest-frontend readme](https://blob.libelektra.org/src/tools/rest-frontend/README.md).

### Homepage & News ###

Besides the documentation we also wanted a place to properly present
ourselves and our news around Elektra. For this reason we created a new
home page which shall give an overview of what Elektra is and can do.
Additionally to that we also added a news section.

We hope that you enjoy our new appearance as much as we do!

### Snippet Sharing ###

Another important part of the website and also without doubt the part
that took most effort to create, is the service that allows for sharing
of configuration snippets. It is run by a REST service fully built with
the help of [CppCMS](http://http://cppcms.com/) on basis of Elektra as
data store. All data concerning snippets and user accounts is stored
in Elektras key database (of course with password being properly hashed).

The service allows you to paste configuration snippets in various (supported)
formats and to tag, describe and name them. This in return allows for easy
search of snippets by keywords and to download them in other formats than
the upload format as well.

Clearly the service is meant to be driven by its users. Therefore we ask
you to share your own configuration snippets, maybe they can be of help
or simply a time saver for someone else!

### NoScript ###

The website is fully written with the help of AngularJS and is therefore
heavily based on JavaScript. This should be no issue though as the
website does only use resources that can be found in the official Elektra
repository. So in case you cannot or do not want to use JavaScript, you
can find all resources also [here](https://git.libelektra.org).

## Feedback ##

At this point there is not much more to say about the new website except for:
Feel free to explore it!

We greatly appreciate all feedback, be it for the website, the snippet sharing
service or other parts of the Elektra project. We always have an open ear
for suggestions and we also like to help with technical issues, simply
[leave us a note on github](https://bugs.libelektra.org)!

## Stay tuned! ##

Subscribe to the reimplemented
[RSS feed](http://www.libelektra.org/rss/feed.rss)
to always get the release notifications.

For any questions and comments, please contact the
[mailing list](https://lists.sourceforge.net/lists/listinfo/registry-list),
use the issue tracker [on github](http://bugs.libelektra.org)
or write an email to elektra@markus-raab.org.
For issues or feedback concerning the website, you can also
contact us at website@libelektra.org.

[Permalink to this NEWS entry](http://www.libelektra.org/rss/102b84a3-c41e-485c-8fe2-f12a24b3fbfd.html)

For more information, see [http://libelektra.org](http://libelektra.org)

Best regards,
Marvin & Markus
