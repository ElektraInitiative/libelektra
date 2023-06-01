/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./visit_context.hpp"
#include <kdb.hpp>

#include <iostream>

class CountryAustriaLayer : public kdb::Layer
{
public:
	std::string id () const
	{
		return "country";
	}
	std::string operator() () const
	{
		return "Austria";
	}
};

class LanguageGermanLayer : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "language";
	}
	std::string operator() () const override
	{
		return "german";
	}
};

class ProfileLayer : public kdb::Layer
{
public:
	ProfileLayer (kdb::visit::Profile const & profile) : m_profile (profile)
	{
	}
	std::string id () const
	{
		return "profile";
	}
	std::string operator() () const
	{
		return m_profile;
	}

private:
	kdb::visit::Profile const & m_profile;
};
//{end}

/*
//{layer/profileOld}
class ProfileLayer : public kdb::Layer
{
public:
	ProfileLayer(kdb::String const & profile) :
		m_profile(profile) {}
	std::string id() const override
	{ return "profile"; }
	std::string operator()() const override
	{ return m_profile; }
private:
	std::string m_profile;
};
//{end}
*/

// application: e.g. git-pull, git-push
//{application}
class MainApplicationLayer : public kdb::Layer
{
public:
	std::string id () const override
	{
		return "application";
	}
	std::string operator() () const override
	{
		return "main";
	}
};

void visit (kdb::visit::Person & p)
{
	p.context ().with<CountryAustriaLayer> ().with<LanguageGermanLayer> () (
		[&] { std::cout << "visit " << ++p.visits << " in " << p.context ()["country"] << ": " << p.greeting << std::endl; });
	std::cout << p.greeting << std::endl;
}

int main ()
{
	using namespace kdb;

	KDB kdb;
	KeySet ks;
	Context c;
	// some predefined values (for convenience):
	ks.append (Key ("user:/visit/%/%/%/person/greeting", KEY_VALUE, "Tag", KEY_END));
	ks.append (Key ("user:/visit/german/Austria/%/person/greeting", KEY_VALUE, "Servus", KEY_END));
	ks.append (Key ("user:/visit/german/switzerland/%/person/greeting", KEY_VALUE, "Grüezi", KEY_END));
	KeySet ks2;
	kdb.get (ks2, "/visit");
	// overwrite them if something is available in config files:
	ks.append (ks2);

	Parameters par (ks, c);

	c.activate<MainApplicationLayer> ();
	c.activate<ProfileLayer> (par.profile);

	for (int i = 0; i < 3; ++i)
		::visit (par.visit.person);

	return 0;
}
