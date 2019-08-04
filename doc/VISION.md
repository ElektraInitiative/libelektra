# VISION

The vision of Elektra is to have
systems in which every configuration
setting is easily accessible and specifiable.

We will use samba and its configuration value
`global/workgroup` as running example.

## Problem

Currently, the administrator would first need to
locate the configuration file, it could be
`/etc/samba/smb.conf`, learn the
[syntax](https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html)
and then implement some script to automatically
replace the value.

## Configuration Management

By a single invocation of a command-line tool
it should be possible to change a configuration
value:

`kdb set system/sw/samba/#0/current/global/workgroup MYGROUP`

If you already use configuration management tools,
the vision is that a single statement suffices to
change a configuration value:

Key/value access in Chef:

```
kdbset 'system/sw/samba/#0/current/global/workgroup' do
	value 'MYGROUP'
	action :create
end
```

Key/value access in Ansible:

```yaml
- name: setup samba workgroup
  connection: local
  hosts: localhost
  tasks:
    - name: set workgroup
      elektra:
        key: "system/sw/samba/#0/current/global/workgroup"
        value: "MYGROUP"
```

    elektra:
      mountpoint: system/sw/samba
      file: /etc/samba/smb.conf
      plugins: ini

Key/value access in puppet-libelektra:

```
kdbkey {'system/sw/samba/#0/current/global/workgroup':
	ensure => 'present',
	value => 'MYGROUP'
}
```

## Legacy

The vision also includes legacy applications which do
not directly use Elektra. Then distributions can mount
configuration files, so that the configuration is
visible within Elektra.

Mounting can also be done via configuration management
tools.

Mounting in puppet-libelektra:

```
kdbmount {'system/sw/samba/#0/current':
	ensure => 'present',
	file => '/etc/samba/smb.conf',
	plugins => 'ini'
}
kdbkey {'system/sw/samba/global/log level':
	ensure => 'absent'
}
```

## Specifications

Key/value specifications in puppet-libelektra:

```
kdbkey {'system/sw/samba/#0/current/global/log level':
	ensure => 'present',
	value => 'MYGROUP',
	check => {
		'type' => 'short',
		'range' => '0-10',
		'default' => '1',
		'description' => 'Sets the amount of log/
			debug messages that are sent to the
			log file. 0 is none, 3 is consider-
			able.'
}
```

Ideally, applications already specify their settings.

## Unique Key Names

The main technique to achieve the vision is
unique key names: Every configuration setting
can be addressed by its unique key name.

With this unique key name, we get an identifier,
which can be used at all places throughout the
system.

## Common Data Structure

## Abstract Configuration For New Applications

## Integrate Legacy Applications

## Specify Keys

## Facilitate Configuration Management

- Continue reading [big picture](BIGPICTURE.md)
