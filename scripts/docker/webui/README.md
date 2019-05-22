# Elektra Web Docker Images

- `elektra/web-base` - base image for elektra web, all other images build upon this (builds elektra with `yajl` and `kdb`)
- `elektra/elektrad` - image that only starts elektrad
- `elektra/webd` - image that only starts webd
- `elektra/elektrad-demo` - same as `elektrad`, but with a KDB config set up (for demo, should run on http://elektrad-demo.libelektra.org)
- `elektra/webd-demo` - same as `webd`, but with 2 instances already created, they both connect to http://elektrad-demo.libelektra.org with different visibility levels (for demo, should run on http://webui.libelektra.org)
- `elektra/web` - image that starts elektrad & webd (for those who just want to try out Elektra Web locally, also mentioned in quickstart in README of #2099)

## Manually Building Docker Images

Make sure to change `1.6` to a new version if it was updated. Base image (bold)
has to be built first, others can be ran in parallel:

- **in `scripts/docker/webui/base` run: `docker build --no-cache -t elektra/web-base:latest -t elektra/web-base:1.6 .`**
- in `scripts/docker/webui/elektrad` run: `docker build -t elektra/elektrad:latest -t elektra/elektrad:1.6 .`
- in `scripts/docker/webui/webd` run: `docker build -t elektra/webd:latest -t elektra/webd:1.6 .`
- in `scripts/docker/webui/web` run: `docker build -t elektra/web:latest -t elektra/web:1.6 .`

To publish these images to docker hub (the main docker registry), simply run:

- `docker push elektra/web-base`
- `docker push elektra/elektrad`
- `docker push elektra/webd`
- `docker push elektra/web`

The `-demo` images are built and deployed automatically by the build server. If
you want to build these manually, you need to specify the `BASE_IMG` environment
variable (can simply be set to `elektra/web-base`).
