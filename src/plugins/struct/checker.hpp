#ifndef CHECKER_HPP
#define CHECKER_HPP


#include <map>
#include <string>
#include <memory>

#include <key>
#include <keyset>

// TODO: remove it
#include <iostream>


namespace elektra {

using namespace kdb;
using namespace std;

class Factory;

class Checker
{
public:
	/**Gets a reference to the outside keyset.
	  Dont change it!
	  Make a duplicate if you need to.*/
	virtual void check(KeySet &ks) = 0;
	/**Build up the Checker.
	  From the Factory you can get instances of other checkers.
	  Will build up recursively, dependent on the given
	  configuration*/
	virtual void buildup (Factory &f, std::string const& templateParameter) = 0;
	virtual ~Checker();
};

class StructChecker : public Checker
{
	KeySet config;
public:
	StructChecker (KeySet config)
		: config(config)
	{}

	void buildup (Factory &f, std::string const& templateParameter);

	void check(KeySet &ks)
	{
		config.rewind();

		Key confRoot = config.next();

		Key cur;
		Key root = ks.next();

		std::cout << "root key is: " << root.getName() << std::endl;


		while (cur = ks.next())
		{
			Key searchKey = config.next();
			if (!searchKey) throw "StructChecker: More keys found than structure should have";
			if (!root.isDirectBelow(cur)) throw "StructChecker: key is not direct below";

			std::cout << "compare basename: " << searchKey.getBaseName() <<
				" with cur: " << cur.getBaseName() << std::endl;
			if (searchKey.getBaseName() != cur.getBaseName())
				throw "StructChecker: did not find expected subkey";

			std::cout << "Apply meta data of " << searchKey.getName() <<
				" to the key " << cur.getName() << std::endl;
			cur.copyAllMeta (searchKey);
		}

		if (config.next()) throw "StructChecker: There should be more elements in the structure";
	}
};

class ListChecker : public Checker
{
	std::auto_ptr<Checker> structure;

public:
	void buildup (Factory &f, std::string const& templateParameter);

	void check (KeySet &ks)
	{
		Key k;
		KeySet ks2 (ks.dup());

		ks2.rewind();
		Key root = ks2.next();

		while (k = ks2.next())
		{
			if (!root.isDirectBelow(k)) throw "ListChecker: key is not direct below";

			KeySet cks(ks2.cut(k));

			structure->check(cks);
		}
	}
};

} // end namespace elektra

#endif
