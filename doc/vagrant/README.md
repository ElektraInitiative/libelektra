# Trying out Elektra with Vagrant
This Vagrantfile provisions a machine based on debian/jessie64 with all tools to build Elektra.

If [Vagrant](https://www.vagrantup.com/) is installed on your machine change to the directory containing the file you are currently reading and build a box for vagrant with
```sh
$ vagrant up && vagrant package && vagrant box add buildelektra package.box && vagrant destroy -f
```
This will take some time, but when its done you have a new vagrantbox as you can verify with `vagrant box list`.
Amongst your boxes you should see the box `buildelektra`.

You can now set up a new VM from this box easily:
Enter a directory where you want to set up the VM
```sh
# enter a directory where you want to set up the VM
$ mkdir ~/vagrant/buildelektra && cd $_
# now you create a Vagrantfile ...
$ vagrant init buildelektra
# ... and start the VM
$ vagrant up
```

When the machine is running, access it with
```sh
$ vagrant ssh
```
In this SSH session you can interact with the machine.

By default Vagrant synchronizes the folder on the host machine containing the vagrantfile with the folder `/vagrant` in the VM.
Therefore we will build a .deb package of Elektra in this folder.

```sh
# in the VM change to the synched folder
$ cd /vagrant
# build the commit with the tag "0.8.19"
$ sudo buildelektra -b elektra 0.8.19
```

When you are done leave the VM with `CTRL-D`.
The folder should now contain the created .deb file.

You can either shut the VM down with `vagrant halt` or delete it with `vagrant destroy`.
