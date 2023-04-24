#include "./location.hpp"
#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::test;

void greet (Person<ContextPolicyIs<ThreadContext>> & p, Country<ContextPolicyIs<ThreadContext>> & country,
	    Location<ContextPolicyIs<ThreadContext>> & location)
{
	cout << p.context ().evaluate (p.greeting.getSpec ().getName ()) << endl;
	p.context ().activate (country);
	p.context ().activate (location);
	cout << p.context ().evaluate (p.greeting.getSpec ().getName ()) << endl;
	cout << p.context ().evaluate (country.getSpec ().getName ()) << endl;
	cout << p.context ().evaluate (location.getSpec ().getName ()) << endl;
	cout << p.greeting << endl;
}

int main ()
{
	Coordinator c;
	ThreadContext tc (c);
	KeySet ks;
	// some predefined values (for convenience):
	ks.append (Key ("user:/test/location", KEY_VALUE, "48N16O", KEY_END));
	ks.append (Key ("user:/test/48N16O/country", KEY_VALUE, "austria", KEY_END));
	ks.append (Key ("user:/test/person/austria/greeting", KEY_VALUE, "Griaz Enk!", KEY_END));
	Environment<ContextPolicyIs<ThreadContext>> env (ks, tc);

	greet (env.test.person, env.country, env.location);
}
