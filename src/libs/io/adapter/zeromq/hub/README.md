# ZeroMq message hub

This is a lightweight message hub for distributing notifications for zeromqsend & zeromqrecv plugins.
This hub is intended for local use via the IPC transport or in a controlled network environment using the TCP transport.
The hub does not feature authentication or encryption.

## Configuration

The endpoints the hub binds to can be configured using the `/sw/elektra/hub_zeromq/#0/current/bind_xsub` and `/sw/elektra/hub_zeromq/#0/current/bind_xpub` configuration keys in KDB.
The default settings are `tcp://127.0.0.1:6000` for XSUB and `tcp://127.0.0.1:6001` for XPUB.
