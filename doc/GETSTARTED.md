# Get Started

- Skill requirements
 - Working with  Linux
 
   We will need a linux-based operating system to run Elektra. If you are new to the world of linux we recommend you to use 
   one of the common linux distributions like [Ubuntu](https://ubuntu.com/download/desktop#download), 
   [Linux Mint](https://www.linuxmint.com/download.php), [openSuse](https://software.opensuse.org/distributions/leap) or 
   [Fedora](https://getfedora.org/en/). 
 - Using command-line interface and commands
 
   The easiest way to compile, install and use Elektra is by using the terminal. We will introduce the basic commands which you will need to run Elektra for the very first time. 
 - Basic knowledge about git
 
   Dont panic! [GIT](https://git-scm.com/) is a distributed version constrol system to track changes of the source code in a project. We will use a single command of GIT to get 
   the source code of Elektra. 
 - Basic knowledge about make/cmake
 
   Never heard about [make](https://www.gnu.org/software/make/) or [cmake](https://cmake.org/), it’s okey! We will use this build-management tools to compile, 
   link dependencies etc. to build a executable program from the source code.  
 - Furthermore, a programming/scripting language like C, C++, ruby, JavaScript etc.
 
   You will need a programming language to fulfill your needs
 
- Software requirements

   We need to install some basic tools to run Elektra: cmake3 , git , and essential build tools (make, gcc, and some standard Unix tools; alternatively ninja and clang are 
   also supported but not described here). Depending on your linux distribution use following commands to install these tools:
   ```sh
   sudo apt-get install cmake git build-essential
   ```
   Or on RPM (Red Hat Package Manager) based systems (like Fedora, openSuse, CentOS etc.):
   ```sh
   sudo yum install -y cmake3 git gcc-c++ 
   ```
   Or on macOS Sierra, most of the build tools can be obtained by installing Xcode (from the App Store). Other required tools may be installed using [brew ](https://brew.sh/). First install brew as 
   described on their website. Then issue the following command to get cmake to complete the basic requirements:
   ```sh
   brew install cmake git 
   ```
- Installation

  If you meet all of the software requirements you can get the source code of Elektra by using this command:
  ```sh
   git clone https://github.com/ElektraInitiative/libelektra.git 
   ```
   Run the following commands to compile Elektra with non-experimental parts where your system happens to fulfil the dependences:
   ```sh
   cd libelektra
   mkdir build
   cd build
   cmake ..  # watch output to see if everything needed is included
   ccmake .. # optional: overview of the available build settings (needs cmake-curses-gui)
   make -j 5
   make run_nokdbtests  # optional: run tests
   ```
   
    After you completed building Elektra on your own, you can execute these commands to install Elektra:
    ```sh
    sudo make install
    sudo ldconfig
   ```
- Hello World!

  Start with your very first Elektra application and follow these steps:
  [Hello World!](doc/tutorials/hello-elektra.md)