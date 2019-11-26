# FreeBSD Vagrant VM

The [Vagrantfile](Vagrantfile) in this directory provides a virtual machine environment that lets you build and run Elektra on FreeBSD.

## Usage

1. Download and install [Vagrant](https://www.vagrantup.com)
2. Download and install [VirtualBox](https://www.virtualbox.org)
3. Change into the directory of the Vagrantfile:

   ```sh
   cd scripts/vagrant/freebsd
   ```

4. Create and configure the guest machine:

   ```sh
   vagrant up
   ```

5. Log into the virtual machine with SSH:

   ```sh
   vagrant ssh
   ```

6. The root of the repository is accessible via `/elektra` in the guest machine:

   ```sh
   ls /elektra
   ```

7. To keep the folder `/elektra` in the virtual machine and the repository on the host computer in sync you can use the following command:

   ```sh
   vagrant rsync-auto
   ```

8. After you logged into the virtual machine you can configure and build Elektra:

   ```sh
   mkdir build
   cmake -GNinja -Bbuild /elektra
   cmake --build build
   ```
