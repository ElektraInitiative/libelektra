# ZeroMQ message hub

The ZeroMQ message hub is a lightweight message hub for distributing
notifications for "zeromqsend" and "zeromqrecv" plugins.
This hub is intended for local use via the IPC transport or in a controlled
network environment using the TCP transport.
The hub does not feature authentication or encryption.

## Usage

The hub can be controlled using the CLI commands "kdb run-hub-zeromq" for
starting and "kdb stop-hub-zeromq" for stopping.

## Configuration

The endpoints the hub binds to can be configured using the
"/sw/elektra/hub-zeromq/#0/current/bind_xsub" and
"/sw/elektra/hub-zeromq/#0/current/bind_xpub" configuration keys in KDB.

The default settings match the default settings for the zeromq plugins which
are "tcp://127.0.0.1:6000" for XSUB and "tcp://127.0.0.1:6001" for XPUB.
