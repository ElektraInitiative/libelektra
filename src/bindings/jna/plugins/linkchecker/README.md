# Link Checker Java JNI Plugin

Found in [LinkPlugin.java](src/main/java/org/libelektra/plugin/LinkPlugin.java).

This class can be used to validate link via the `check/link` metakey.

Initially it uses a Java standard library function (`URLConnection`) to check if the link is reachable.

You can supply an optional timeout for the reachability check.
If no timeout or an empty timeout is provided, the plugins defaults to
checking the validity of the URL via `URL` and its `MalformedURLException`.

#### Example usage

Check the reachability of a link for 200ms.
If the link cannot be reached then check the validity of the url.

```sh
kdb meta-set spec:/sw/app/current/\#0/server/link check/link 200
```

Try to set an invalid link:

```sh
kdb set -N user -- /sw/app/current/\#0/server/link ht214:/invalid.org
#> Found invalid or unreachable links: user:/sw/app/current/#0/server/link
```

#### Current Limitations

- Only checks the connection during `kdbSet()` operations.
