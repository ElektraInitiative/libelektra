# elektra-granularity(7) -- relation of keys to files

Keys of a backend can only be retrieved as a full key set. Currently, it
is not possible to fetch a part of the keys of a backend. So the user
needs to cut out the interesting keys with `ksCut()` afterwards.
If the keys should be committed again, the whole key set must be
preserved. Otherwise, the clipped keys will be removed permanently.

This restriction simplifies storage plugins while it does not limit the
user. Other plugins would not be able to fulfil their purpose without
the full `KeySet`. For example, a plugin that checks the availability
and structure of all keys cannot work with a partial key set.

It is problematic to have too many keys in one backend. The applications
would need memory for unnecessary configuration data. Instead, we
recommend introducing several mount points to split up the keys into
different backends. Splitting up key sets makes sense if any application
requests only a part of the configuration. No benefits arise if every
application requests all keys anyway.

Let us assume that many keys reside in `user:/sw` and an
application only needs the keys in `user:/sw/org/app`. To save memory
and get better startup-times for the application, a new backend can be
mounted at `user:/sw/org/app`. On the other hand, every mounted backend
causes a small run-time overhead in the overall configuration system.

The solution in Elektra is flexible, because the user decides the
granularity. It is possible to mount a backend on every single key, so
that every key can be requested for itself. If no backends are mounted,
all keys reside in the default backend.

To sum up, Elektra’s core searches for the nearest mount point and gets
the configuration from there. It is possible that the user gets more
configuration than requested. The user can decide by means of mounting
how much configuration on specific requests are returned.
