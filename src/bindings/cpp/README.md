- infos =
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/status = maintained nodep
- infos/provides =
- infos/description =

The C++ binding is a 1:1 mapping of all C-functions into C++. In this
README the advantages are described.

## Installation

See [installation](/doc/INSTALL.md).
The package containing the header files is called `libelektra-dev`.

## No Explicit Delete Necessary

The objects will automatically be freed when they leave the scope. E.g.

```cpp
int main() {
	Key k;
	KDB kdb;
	KeySet ks;
} // k, kdb and ks will automatically be freed here
```

The references are automatically tracked whenever a key is appended or
removed to a KeySet. This takes away the major causes of memleaks.

## Exceptions and IO

No return values need to be checked. Instead on problems an exception is
thrown:

```cpp
try
{
	kdb::KDB kdb(k);
	kdb.get(ks, k);
	kdb.set(ks, k);
  // Continued below ↓
  // …
}
```

We do not have to care here that something might not work, because an
exception will be thrown in these cases. Additionally, it is very
simple to print a keyset:

```cpp
std::cout << ks;
```

At the end we close KDB explicitly, so that we also get the warnings
from there. The C++ binding then features a possibility to print the
warnings.

```cpp
  // …
  // Continuation of code above ↑
	kdb.close(k);
	printWarnings(std::cout, k, true, true);
}
catch (kdb::KDBException const & e)
{
	std::cout << e.what();
}
```

See [here for the full code](examples/cpp_example_io.cpp).
e.what() will print all warnings and errors by default.
This can be [customized](examples/cpp_example_userio.cpp).

It can also be changed which exceptions are [thrown as shown
here](examples/cpp_example_userexception.cpp).

## Allow Iterations

Next to the C-style fashioned loop:

```cpp
for (elektraCursor it = 0; it < ks.size (); ++it)
{
	std::cout << ks.at (it).getName() << std::endl;
}
```

The C++ interface also supports real iterators:

```cpp
for (KeySet::iterator i = ks3.begin(); i != ks3.end(); ++i)
{
	Key k(*i);
	std::cout << k.getName() << std::endl;
}
```

Also C++11 iterators are supported, and of course reverse and const
iterators, too.

## Type Safety

[Contextual Values](include/contextual.hpp) allow a key to be used as
native variables, see [here how they can be
generated](/src/tools/pythongen).
