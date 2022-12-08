# VISION

The vision of Elektra is to have systems in which every configuration
setting is easily accessible and specifiable.

We will use Samba and its configuration value `global/workgroup` as
running example.

## Problem

Currently, the administrator would first need to
locate the configuration file, it
could be `/etc/samba/smb.conf`, learn the
[syntax](https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html)
and then implement some configuration management code to add or replace
the value. Furthermore, application developers need to implement
the parser, design tools like [SWAT](https://www.samba.org/samba/docs/old/Samba3-HOWTO/SWAT.html),
and document in detail how to configure their specific software.
This is a huge effort on both sides.

## Configuration Management

We envision, that instead a key-value interface
allows us to change any configuration value
as desired.

Either by either invoking [command-line tools](/doc/help/kdb.md):

```sh
kdb set system:/sw/samba/#0/current/global/workgroup MYGROUP
```

Also by importing an INI file with the information:

```ini
kdb import system:/sw/samba/#0/current ini << HERE
[global]
workgroup=MYGROUP
HERE
```

Or using some compiled language like C
(shown code uses the [high-level API](/src/libs/highlevel),
with neither code generation nor error handling):

```c
#include <elektra/elektra.h>

int main ()
{
	ElektraError* error;
	Elektra * elektra = elektraOpen ("system:/sw/samba/#0/current", 0, &error);
	elektraSetString (elektra, "global/workgroup", "MYGROUP", &error);
	elektraClose (elektra);
}
```

Or using some interpreted language like Python
(shown code uses the [Python binding of the low-level API](/doc/tutorials/python-kdb.md)):

```python
import kdb
k = kdb.KDB()
ks = kdb.KeySet()
s = "system:/sw/samba/#0/current"
k.get (ks, s)
ks.append(kdb.Key(s+"/global/workgroup", kdb.KEY_VALUE, "MYGROUP"))
k.set (ks, s)
```

Or if you already use configuration management tools, the vision is that
a single statement within the configuration management tool suffices to
change a configuration value.

Key-value access in [puppet-libelektra](https://puppet.libelektra.org):

```
kdbkey {'system:/sw/samba/#0/current/global/workgroup':
	ensure => 'present',
	value => 'MYGROUP'
}
```

Key-value access in Chef:

```
kdbset 'system:/sw/samba/#0/current/global/workgroup' do
	value 'MYGROUP'
	action :create
end
```

Key-value access in Ansible:

```yaml
- name: setup samba workgroup
  connection: local
  hosts: localhost
  collections:
    - elektra_initiative.libelektra
  tasks:
    - name: set workgroup
      elektra:
        mountpoint: system:/sw/samba/#0/current/global
        keys:
          workgroup:
            value: MYGROUP
```

In all these examples, we have set
`system:/sw/samba/#0/current/global/workgroup` to `MYGROUP`.

## Application Integration

Different to other solutions, in Elektra
[applications itself can be integrated](/doc/tutorials/application-integration.md),
too.
In Samba we would simply replace the configuration file parser
with code like ([low-level C code](https://doc.libelektra.org/api/latest/html/group__key.html),
no error-handling and no cleanup):

```c
#include <elektra/kdb.h>
#include <stdio.h>

int main (void)
{
	KeySet * myConfig = ksNew (0, KS_END);
	Key * key = keyNew ("/sw/samba/#0/current", KEY_END);
	KDB * handle = kdbOpen (NULL, key);
	kdbGet (handle, myConfig, key);

	Key * result = ksLookupByName (myConfig, "/sw/samba/#0/current/global/workgroup", 0);
	printf ("My workgroup is %s", keyString (result));
}

```

If any of the codes above were executed, the
application will print `My workgroup is MYGROUP`.

Applications that were modified to directly use Elektra
are called to be `elektrified`.

But the vision also includes legacy applications which do
not directly use Elektra. Then distributions can mount
configuration files, so that the configuration is
visible within Elektra.

Elektra uses the same configuration to configure itself, so every way
shown above can be used. To make mounting more simple, we introduced an
extra tool:

```sh
kdb mount /etc/samba/smb.conf system:/sw/samba/#0/current ini
```

Mounting can also be done via configuration management
tools.

Mounting in puppet-libelektra:

```
kdbmount {'system:/sw/samba/#0/current':
	ensure => 'present',
	file => '/etc/samba/smb.conf',
	plugins => 'ini'
}
kdbkey {'system:/sw/samba/global/log level':
	ensure => 'absent'
}
```

Mounting in Ansible:

```yaml
- name: setup samba workgroup
  connection: local
  hosts: localhost
  tasks:
    - name: set workgroup
    elektra:
      mountpoint: system:/sw/samba
      file: /etc/samba/smb.conf
      plugins: ini
```

## Specifications

Ideally, applications specify their settings.
This way, we know which keys exist on a system and
which values are expected for these keys.
Then administrators do not need to guess.

Next to the documentation for humans, specifications
also provide information for software.
For example, the [Web UI](/src/tools/webui) automatically
gives input fields according to the type of
the configuration setting. For example, a boolean
configuration setting gets a checkbox.

Also the specifications are integrated within
Elektra in the same way. Again, we can use any
of the above ways to specify configuration.

Either by either invoking [command-line tools](/doc/help/kdb.md):

```sh
kdb set-meta /sw/samba/#0/current/global/workgroup type string
kdb set-meta /sw/samba/#0/current/global/workgroup description "This controls what workgroup your server will appear to be in when queried by clients. Note that this parameter also controls the Domain name used with the security = domain setting."
```

Key-value specifications in [puppet-libelektra](https://puppet.libelektra.org):

```
kdbkey {'system:/sw/samba/#0/current/global/workgroup':
	ensure => 'present',
	check => {
		'type' => 'string',
		'default' => 'WORKGROUP',
		'description' => 'This controls what workgroup
			your server will appear to be in when
			queried by clients. Note that this
			parameter also controls the Domain
			name used with the security = domain
			setting.'
}
```

Note, that the specification (in both examples above) actually lands up in
`spec:/sw/samba/#0/current/global/workgroup`. The unique path to the
configuration setting is `/sw/samba/#0/current/global/workgroup`, but
the specification gets written to the [namespace](/doc/tutorials/namespaces.md)
`spec`, while the system-configuration gets written to the namespace `system`.

## Conclusion

Here we have shown how Elektra can be used
to configure systems more easily.
Both application developers and administrators
can save time.

The main technique to achieve the vision is
unique key names: Every configuration setting
can be addressed by its unique key name.

With this unique key name, we get an identifier,
which can be used at all places throughout the
system.

- Continue reading [big picture](BIGPICTURE.md)
