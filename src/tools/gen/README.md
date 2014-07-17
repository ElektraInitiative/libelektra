# Code Generation

This tutorial guides you into how you can use the code generator and
contextual values.

## Motivation

Writing your own conversion of the strings parsed from commandline
arguments and configuration files to native variables is error-prone and
time consuming, e.g.:

	k = ksLookupByName(ks, "user/app/current/number");
	int i = 20;
	if (k) i = atoi(keyString(k));

We see already multiple problems with this code:

- Name of key encoded in program (could be wrong/undocumented)
- Default value (20) is encoded in application (could be undocumented)
- No error handling for null/binary keys
- No error handling when string is not an integer

Our approach allows to avoid all such errors.


## Specification

To avoid initial stated problems, we use [a specification](specification.ini).
From that specification code is generated similar to the error-prone and
time consuming code above, but without its problems.
To generate the code we use:

	kdb gen specification.ini template_context.hpp > lift_context.hpp

The usage of the generated code is very easy, we just create a parameter
object:

	kdb::KeySet ks;
	kdb::Context c;
	kdb::Parameters par(ks,c);

and access the keys as if they were variables:

	std::cout << "delay: " << par.test.lift.emergency.delay << std::endl;

For a full example, see [here](lift_context.cpp).


## Contextual Values

The key name often is not the same, but depends on the context.
E.g. if an application is started with another profile:

	firefox -P anonymous

we want different configuration, e.g. the key username should certainly
not give hints of the user. In Elektra such problems are solved by
introduction of another level of hierarchy:

	user/firefox/default/username = Max Mustermann
	user/firefox/anonymous/username = Anonymous User

So the -P commandline option puts the program into another context:
keys below "user/firefox/anonymous" will be used then.

To define such a contextual value, we change the specification to
contain placeholders, e.g.:


	[/firefox/%profile%/username]
	type=string

The placeholder %profile% will be replaced by the current profile.
Now we need a so-called layer to actually switch the profile:

	class ProfileLayer : public kdb::Layer
	{
	public:
		ProfileLayer(std::string const & profile) :
			m_profile(profile) {}
		std::string id() const { return "profile"; }
		std::string operator()() const { return m_profile; }
	private:
		std::string m_profile;
	};

The id() has to match the placeholder we saw before. Whatever operator()
yields will be used then. Then we can easily switch between profiles:

	par.activate<ProfileLayer>("anonymous");

Which makes sure that all contextual values that contain the placeholder
%profile% will use the key from anonymous afterwards.


## Commandline Arguments

Now we want to implement the -P commandline option which is trivial
using Elektra's code generator:

	[/firefox/profile]
	type=string
	opt=P
	opt/long=profile

The specification entries "opt" and "opt/long" will generate, next to
the contextual value firefox.profile additional code parsing the
commandline:

	kdb gen specification.ini template_genopt.c > genopt.c

To parse all arguments as defined in the specification, we simply use
the generated function:

	kdb::ksGetOpt(argc, argv, ks); 

Then we can implement firefox-like profiles as shown
[here](visit_context.cpp):

	par.activate<ProfileLayer>(par.profile);
