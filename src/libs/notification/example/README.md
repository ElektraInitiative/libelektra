# Example Applications for Notifications

This folder contains two example notifications:

- example_notification: Repeatedly calls kdbGet, does not require transport plugins
- example_notification_async: Uses asynchronous I/O. Requires transport plugins

Both applications use the same keys:

- /sw/tests/example_notification/#0/current/value: Set to any integer value
- /sw/tests/example_notification/#0/current/color: Set the text color. Valid
  values are "red", "green" and "blue".

## "example_notification"

Is always built with the notification library.

## "example_notification_async"

Requires:

- Binding: `io_uv`

Usage:

Make sure that the required transport plugins are mounted (e.g. for D-Bus):

> kdb global-mount dbus dbusrecv
