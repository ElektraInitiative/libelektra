# Get Started

- Skill requirements
  - Operating system
 
    We will need a unix-based operating system to run Elektra (Linux, BSD, macOS) but it's also possible to use Windows which is supported but not tested much. If you are new to the world of unix we recommend you to use 
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
  - (optional) Furthermore, a programming/scripting language like C, C++, ruby, JavaScript etc. ...
 
    ... if you are planning to contribute to Elektra.
 
- Software requirements

  We need to install some basic tools to run Elektra: cmake , git , and essential build tools (make, gcc, and some standard Unix tools; alternatively ninja and clang are 
  also supported but not described here). Depending on your linux distribution use following commands to install these tools:
  ```sh
  sudo apt-get install cmake git build-essential
  ```
  Or on RPM (Red Hat Package Manager) based systems (like Fedora, openSUSE, CentOS etc.):
  ```sh
  sudo yum install -y cmake git gcc-c++ 
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
  cd libelektra  #navigate to libelektra
  mkdir build  && cd build  #create and navigate to the build directory
  cmake ..  # watch output to see if everything needed is included
  ccmake .. # optional: overview of the available build settings (needs cmake-curses-gui)
  make -j 5  # running makefile (compiling, linking etc.), -j specifies the number of jobs to run simultaneously
  make run_nokdbtests  # optional: run tests
  ```
  With these commands you will be able to run the "Hello World!" example but usually you will need to use some of the plugins, tools and bindings of Elektra. Please take a look to the more detailed [compiling documentation](/doc/COMPILE.md) .
  After you completed building Elektra on your own, you can execute these commands to install Elektra (please check the [installation documentation](/doc/INSTALL.md) if you are using Debian):
  ```sh
  sudo make install
  sudo ldconfig #optional: check installation documentation for more information
  ```
  [Installation documentation](/doc/INSTALL.md) contains further information about available packages.
   
- Hello World!

  Start with your very first Elektra application and follow these steps:
  [Hello World!](/doc/tutorials/hello-elektra.md)