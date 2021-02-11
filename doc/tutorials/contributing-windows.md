# Introduction

In this tutorial, you will learn how to set up a development environment for Elektra using Windows Subsystem for Linux (WSL) and Visual Studio 2019.

Visual Studio 2019 (VS2019) Community is free but requires a registration after a couple of days.
[Download it](https://visualstudio.microsoft.com/downloads/)
and install the Linux development with C++ workload.

Elektra development mainly happens on GNU/Linux distributions.
Therefore, it makes sense to use WSL if you have Windows 10 installed on your PC.
For the beginning, it does not matter if you choose WSL 1 or WSL 2.
It is possible to convert them later.

# Download and Installation

We assume you use Ubuntu as WSL distribution.
Install the packages required for building from VS2019:

```sh
apt install g++ gdb make ninja-build rsync zip
```

Then clone the libelektra git repository:

```sh
git clone git@github.com:ElektraInitiative/libelektra.git
```

> **Do not use VS2019 to clone the repository**: You will not be able to compile because of invalid line endings.

and open it in VS2019.
It will complain that IntelliSense is out of date.
In addition, the `Error List` at the bottom will show CMake errors about missing compilers.
To solve this, you have to configure a Linux CMake project.

# Configure a Linux CMake project

Microsoft has [even more information](https://docs.microsoft.com/en-us/cpp/linux/cmake-linux-configure?view=msvc-160) about this task.

In the drop-down menu that says `x64-Debug (Default)` click `Manage Configurations...`.
Add `WSL-GCC-Debug` and save the configuration.

> You can remove `x64-Debug (Default)`.

CMake generation will automatically restart.
When it has completed, you can click the drop-down `Select Startup Item...` (by the green _Play_ button).

A long list of targets should appear.
Choose `hello (bin\hello)` and click the green _Play_ button.
You should now see the output `Hello world` in the Linux Console Window at the bottom.

# Why choose VS2019

One advantage of using VS2019 is the graphical debugger.
Search for `hello.c` in the Solution Explorer to the right.
Create a breakpoint in the main function and run the program again using the green _Play_ button.

On the bottom-left you will see the windows `Autos`, `Locals` and `Watch 1`.
Click on `Locals` to inspect the `Key * k`.
Right-click `keyNew` to access functions like `Peek Definition`.

# Choosing your WSL version

It makes sense to compare WSL 1 and WSL 2 and make an informed decision for one of them.
Your setup may require adaptions, such as installing an SSH server in your Linux distribution.

Good sources of information are:

- the article [Comparing WSL 1 and WSL 2](https://docs.microsoft.com/en-us/windows/wsl/compare-versions) from the official WSL documentation
- the blog post [C++ with Visual Studio and WSL2](https://devblogs.microsoft.com/cppblog/c-with-visual-studio-and-wsl2/)
- the official [Linux with Visual Studio C++ documentation](https://docs.microsoft.com/en-us/cpp/linux/?view=msvc-160)

Commands to change your WSL version can be found, for example, in the blog post
[WSL 2 is now available in Windows Insiders](https://devblogs.microsoft.com/commandline/wsl-2-is-now-available-in-windows-insiders/)

# Troubleshooting

For further information, consider the official
[Linux with Visual Studio C++ documentation](https://docs.microsoft.com/en-us/cpp/linux/?view=msvc-160).
