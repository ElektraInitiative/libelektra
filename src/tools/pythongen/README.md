# kdb-gen(1) -- code generation

This tutorial serves as a guide for how you can use the code generator and
contextual values. For more background read
[this paper](http://www.markus-raab.org/ftp/papers/cop2014program.pdf).

## Motivation

Writing your own conversion of the strings parsed from commandline
arguments and configuration files to native variables is error-prone and
time consuming to code, e.g.:

```c
k = ksLookupByName(ks, "user:/app/current/number");
int i = 20;
if (k) i = atoi(keyString(k));
```

We see already multiple problems with this code:

- Name of key encoded in program (could be wrong/undocumented)
- Default value (20) is encoded in application (could be undocumented)
- No error handling for null/binary keys
- No error handling when string is not an integer

Our approach avoids all such errors and saves time for more
important implementation tasks.

## Specification

To avoid the problems we initially stated, we use [a specification](tests/lift.ini).
Using the specification, we can generate code similar to the code above, but without any of the errors.
To generate the code we use:

```sh
kdb gen specification.ini template_context.hpp -o lift_context.hpp
```

Using the generated code is very easy, we just create a parameter
object:

```cpp
kdb::KeySet ks;
kdb::Context c;
kdb::Parameters par(ks,c);
```

and access the keys as if they were variables:

```cpp
std::cout << "delay: " << par.test.lift.emergency.delay << std::endl;
```

For a full example, see [here](tests/lift.cpp), or [here for a thread-safe version](tests/lift_context.cpp).

## Contextual Values

The value of a key often depends on a context.
E.g. if an application is started with another profile:

```sh
firefox -P anonymous
```

we want different configuration values,
e.g. the value of the key `/username` should certainly
not give hints of the user. In Elektra such problems are solved by
introduction of an additional level in the hierarchy.
So instead of:

```ini
user:/firefox/username = Max Mustermann
```

We can have multiple values for the same key:

```ini
user:/firefox/default/username = Max Mustermann
user:/firefox/anonymous/username = Anonymous User
```

To define such a contextual value, we change the specification to
contain placeholders, e.g.:

```ini
[/firefox/%profile%/username]
type=string
default=none
```

Note that the current implementation always requires a default
value to be present which will be used when no configuration could
be loaded.

The placeholder %profile% will be replaced by the current profile.
Now we need a so-called layer to actually switch the profile:

```cpp
class ProfileLayer : public kdb::Layer
{
public:
	ProfileLayer(std::string const & profile) :
		m_profile(profile) {};
	std::string id() const { return "profile"; }
	std::string operator()() const { return m_profile; }
private:
	std::string m_profile;
};
```

The id() has to match the placeholder we saw before. Whatever operator()
yields will be used instead of this placeholder.
Now we have everything ready to actually switch profiles:

```cpp
par.activate<ProfileLayer>("anonymous");
```

The library function `activate`
makes sure that all contextual values that contain the placeholder
%profile% will use "anonymous" instead of the placeholder afterwards.
If no placeholder exists `%` will be used.

## Command-line Options

Now if we want to implement the -P commandline option, we can do so very
easily using Elektra’s code generator. We simply add another
item in the specification:

```ini
[/firefox/profile]
type=string
opt=P
opt/long=profile
default=none
```

The specification entries "opt" and "opt/long" will generate, next to
the contextual value `firefox.profile` additional code parsing can be
done via the commandline:

```sh
kdb gen specification.ini template_genopt.c -o genopt.c
```

To parse all arguments as defined in the specification, we simply use
the generated function:

```cpp
kdb::ksGetOpt(argc, argv, ks);
```

Then we can implement firefox-like profiles as shown
[here](tests/visit_context.cpp) by activating what we got from
commandline:

```cpp
par.activate<ProfileLayer>(par.profile);
```

## Limitations

This is an experimental prototype. You should have a look at the
test cases to see what is actually working.

The code generator has little (none) checks for valid specifications.
For problems in the specification you usually get hard-to-interpret
compile errors.

Not all of Elektra's types are supported, for example, char and octet
do not work.

Please open an issue if you plan to use the code.

## See Also

If you want to know more read:

- [introduction paper](http://www.markus-raab.org/ftp/papers/cop2014program.pdf).
- [multi-threaded extension](http://www.markus-raab.org/ftp/papers/seus2015global.pdf)
- [multi-process extension](http://www.markus-raab.org/ftp/papers/mobile2016persistent.pdf)
