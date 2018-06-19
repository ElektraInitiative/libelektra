# Elektra Web docker images

- `elektra/web-base` - base image for elektra web, all other images build upon this (builds elektra with `yajl` and `kdb`)
- `elektra/elektrad` - image that only starts elektrad
- `elektra/webd` - image that only starts webd
- `elektra/elektrad-demo` - same as `elektrad`, but with a KDB config set up (for demo, should run on http://elektrad-demo.libelektra.org)
- `elektra/webd-demo` - same as `webd`, but with 2 instances already created, they both connect to http://elektrad-demo.libelektra.org with different visibility levels (for demo, should run on http://webui.libelektra.org)
- `elektra/web` - image that starts elektrad & webd (for those who just want to try out Elektra Web locally, also mentioned in quickstart in README of #2099)

