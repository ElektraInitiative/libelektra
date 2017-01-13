# Trying out Elektra with Vagrant
This Vagrantfile provisions a machine based on debian/jessie64 with all tools to build Elektra.

If [Vagrant](https://www.vagrantup.com/) is installed on your machine change to the directory containing the file you are currently reading and start the VM with
```bash
$ vagrant up
```
When you run this command for the first time this will take some time as Vagrant will install all required packages.

When the machine is ready and running, access it with
```bash
$ vagrant ssh
```
In this SSH session you can interact with the machine.

You could for instance change to `/vagrant`, which contains the contents of `share` from the host machine.

Therefore it should contain an exemplary shell script that pulls a specified snapshot of Elektra from Github, installs it in the VM and builds a .deb package from it.

Go ahead and give it a try:
```bash
$ cd /vagrant
# this builds the commit with the tag "0.8.19"
$ sudo ./packelektra.sh "0.8.19"
```

After this command has finished you can safely test your specified Elektrasnapshot in the VM.

When you are done leave the VM with `CTRL-D`. Now you can either just shut it down with `vagrant halt` or delete it with `vagrant destroy`.
