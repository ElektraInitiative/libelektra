# elektra-web

_an API and web user interface to remotely manage Elektra instances_

The configuration view of elektra-web is similar to the tree view of the
[qt-gui](https://master.libelektra.org/src/tools/qt-gui), but with
dynamic fields rendered via key metadata.

## Dependencies

Elektra-web requires:

- [Elektra](https://libelektra.org/) with the [`yajl` plugin](https://master.libelektra.org/src/plugins/yajl/) installed
- A recent [node.js](https://nodejs.org/en/) installation (at least 6.x)

## Building with elektra-web Tool

To build Elektra with the elektra-web tool:

- Install Node.js and dependencies for `yajl` plugin (see links above)
- Configure libelektra build with the elektra-web tool, e.g. `cmake .. -DTOOLS="kdb;web"`
- Build libelektra: `make`
- Install libelektra: `sudo make install`

## Getting Started

- Start an elektrad instance: `kdb run-elektrad`
- Start the client: `kdb run-web`
- You can now access the client on: [http://localhost:33334](http://localhost:33334)

## Getting Started (docker)

- Create and run a new docker container: `docker run -d -it -p 33333:33333 -p 33334:33334 elektra/web`
- You can now access the client on: [http://localhost:33334](http://localhost:33334)

## Running from source

- Install dependencies (see above)
- Clone libelektra repo and `cd libelektra/src/tools/web`
- Install and start an elektrad instance:

  - `cd elektrad`
  - `npm install`
  - `npm start` (replaces `kdb run-elektrad`)

- Install and start the client (connects to the elektrad instance):

  - `cd client`
  - `npm install`
  - `npm start` (replaces `kdb run-web`)

- You can now access the client on: [http://localhost:33334](http://localhost:33334)

## Use-cases

### Running elektra-web on a Single Instance

If you do not want to configure multiple instances, you can set the `INSTANCE`
environment variable to the server you want to configure. You can also set
`user/sw/elektra/web/#0/current/instance` to the host. Make sure to enter a full
HTTP URL, e.g. `http://localhost:33333`.

If this configuration option is set, elektra-web will load the configuration
page for that instance instead of the main overview page.

If you want to host elektra-web with the client and elektrad on the same
instance, after starting elektrad via `kdb run-elektrad`, you can run start the
client as follows:

```sh
INSTANCE="http://localhost:33333" kdb run-web
```

It is also possible to set visibility by prefixing the host with `VISIBILITY@`.

For example (`advanced` visibility, `user` is default):

```sh
INSTANCE="advanced@http://localhost:33333" kdb run-web
```

Now, when you open [http://localhost:33334](http://localhost:33334) in your
browser, the configuration page for the instance will be opened immediately.

### Using a Different `kdb` Executable

It is possible to change the `kdb` executable that elektra-web uses by setting
the `KDB` environment variable. Please ensure to use the same `KDB` executable
when starting `elektrad` and the `client`.

For example:

```sh
KDB="/usr/local/custom/bin/kdb" kdb run-elektrad
KDB="/usr/local/custom/bin/kdb" kdb run-web
```

## Overview

Elektra web consists of multiple components:

- (multiple) servers running an elektra daemon ([`elektrad`](elektrad/))
- a single server to communicate with the elektra daemons and serve the client ([`webd`](webd/))
- a web browser that accesses the client (Web UI) on the [`webd`](webd/) server ([`client`](client/))

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/network_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/network_structure.png)

## API

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/daemon_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/daemon_structure.png)

[API blueprints](https://apiblueprint.org/) are available for both APIs:

- [elektrad](https://master.libelektra.org/doc/api_blueprints/elektrad.apib), documentation: https://elektrad.docs.apiary.io/
- [webd](https://master.libelektra.org/doc/api_blueprints/webd.apib), documentation: https://elektrawebd.docs.apiary.io/

## Test REST API on localhost

In order to test API on localhost, you have to start elektrad instance. You can do it in two ways:

- run manually (if you would like to start it manually or you don't have eletrad-web tool installed)

  - `cd libelektra/src/tools/web`
  - `cd elektrad`
  - `npm install`
  - `npm start`

- by installing elektrad tool together with Elektra and run it
  - please see the section `Building with elektra-web Tool`

Now the server is runing on [http://localhost:33333](http://localhost:33333). After that you can test API with help of Postman or other tool, which allows to send REST API requests.

Additional note. It is recommended to install the elektrad tool than starting the server manually.
When Elektra is installed, the `kdb` command together with its tools is installed globally.
For instance, whenever you would like to write any shell script, which has to start a REST API server, you can just add the following line `kdb run-elektrad` inside your file and save it.
After that, the created shell script can be executed from any directory.

Examples:

let's create the new key-value pair `user/test` and set its value to 5. You can do it next way:

- through the command terminal
  ```sh
  kdb set user/test 5
  ```
- through the rest api using curl
  ```sh
  curl -X PUT -H "Content-Type: text/plain" --data "5" http://localhost:33333/kdb/user/test
  ```

The output of any of two commands will be: `Set string to "5"`. If the specified key didn't exist before, then the output will be `Create a new key user/test with string "5"`.

Now, the command

```sh
curl http://localhost:33333/kdb/user/test
```

will return us the value of the specified key `user/test`, which is stored in the database right now

<!-- prettier-ignore-start -->

```json
{
    "exists": true,
    "name": "test",
    "path": "user/test",
    "ls": [
        "user/test"
    ],
    "value": "5",
    "meta": ""
}
```

<!-- prettier-ignore-end -->

## Auth

Currently, webd does not support authentication. The best way to work around
this is to use a reverse proxy (e.g. [nginx reverse proxy](https://www.nginx.com/resources/admin-guide/reverse-proxy/)).

Once you set up a reverse proxy on your web server, you can use it to
authenticate users, e.g. by [username/password auth](https://www.digitalocean.com/community/tutorials/how-to-set-up-password-authentication-with-nginx-on-ubuntu-14-04)

## Code Structure

- `elektrad/` - contains the daemon to interact with a single elektra instance
- `webd/` - contains a daemon to serve the client and interact with multiple elektra instances

- `client/` - contains the elektra-web client (Web UI)

  - `src/actions/` - Redux actions to access the KDB or display notifications in the UI
  - `src/components/` - React components

    - `pages/` - pages in the app

      - `Home.jsx` - the main page (overview of all instances)
      - `Configuration.jsx` - configuration page (single instance)

    - `TreeItem/` - contains all UI components related to a single item in the tree view

      - `dialogs/` - these dialogs are opened when certain actions are pressed (icons next to the tree items)

        - `AddDialog.jsx` - dialog to create a new (sub-)key
        - `DuplicateDialog.jsx` - dialog to duplicate a key
        - `EditDialog.jsx` - dialog to edit a key value
        - `RemoveDialog.jsx` - dialog to confirm the removal of a key
        - `SettingsDialog.jsx` - dialog to edit metadata (new metadata can be implemented here)
        - `*SubDialog.jsx` - sub-dialogs of the SettingsDialog

      - `fields/` - special input fields to display various values

    - `App.jsx` - defines app structure and routes

  - `src/index.js` - main entry point of the app (fetches instances and renders UI)
  - `src/containers/` - contains components that are connected to Redux
  - `src/css/` - contains CSS styles
  - `src/reducers/` - contains Redux reducers (used to process actions)

## Development Guides

### Updating Dependencies

Lockfiles (`package-lock.json`) can be updated by simply deleting the current
lock file and running `npm install`, which creates a new lock file.

Check for outdated dependencies via `npm outdated`. Dependencies can then be
updated by running `npm update`.

### Building Docker Image

Run the following command in the `scripts/docker/web/` directory, replacing `1.5.0` with the latest version:

```sh
docker build -t elektra/web:1.5.0 -t elektra/web:latest .
```

Test the image:

```sh
docker run -d -it -p 33333:33333 -p 33334:33334 elektra/web:1.5.0
```

Publish it to the docker registry:

```sh
docker push elektra/web:1.5.0
```

### Adding Support for New Metadata

- Create a new sub dialog by, for example, copying the `NumberSubDialog.jsx`
  file (or similar) to a new file in the
  `client/src/components/TreeItem/dialogs` folder.

- Include the sub dialog by adding it to the `SettingsDialog.jsx` file in the
  same folder. For example, it could be added before the
  AdditionalMetakeysSubDialog at the end of the file:

```diff
+     <NewSubDialog
+       onChange={this.handleEdit('check/something')}
+       value={this.getMeta('check/something', '')}
+       saved={this.getSaved('check/something')}
+     />
      <AdditionalMetakeysSubDialog
        handleEdit={this.handleEdit.bind(this)}
        getMeta={this.getMeta.bind(this)}
        getSaved={this.getSaved.bind(this)}
        meta={this.props.meta}
        deleteMeta={this.props.deleteMeta}
      />
    </FocusTrapDialog>
```

- Mark the meta keys as handled by adding them to the `HANDLED_METADATA` array
  in `client/src/components/TreeItem/dialogs/utils.js`:

```diff
export const HANDLED_METADATA = [
  ...,
  'visibility',
  'binary',
+ 'check/something',
]
```

- Validation can then be added by handling metadata in the
  `client/src/components/TreeItem/fields/validateType.js` file to the
  `validateType` function.

- Rendering fields in a special way when certain metakeys are present can be
  done by adjusting the `renderSpecialValue` function in the `client/src/components/TreeItem/index.js` file.
