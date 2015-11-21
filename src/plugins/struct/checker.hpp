/**
 * @file
 *
 * @brief Headerfile of Struct checker
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef CHECKER_HPP
#define CHECKER_HPP


#include <map>
#include <string>
#include <memory>

#include <key.hpp>
#include <keyset.hpp>


namespace elektra {

class Factory;

class Checker
{
public:
	/**Gets a reference to the outside keyset.
	  Dont change it!
	  Make a duplicate if you need to.*/
	virtual void check(kdb::KeySet &ks) = 0;
	/**Build up the Checker.
	  From the Factory you can get instances of other checkers.
	  Will build up recursively, dependent on the given
	  configuration*/
	virtual void buildup (Factory &f, std::string const& templateParameter) = 0;
	virtual ~Checker();
};

typedef std::unique_ptr<Checker> CheckerPtr;

class StructChecker : public Checker
{
	kdb::KeySet config;
public:
	StructChecker (kdb::KeySet config_)
		: config(config_)
	{}

	void buildup (Factory &f, std::string const& templateParameter) override;

	void check(kdb::KeySet &ks) override
	{
		config.rewind();

		kdb::Key confRoot = config.next();
		if (!confRoot) throw "StructChecker: No confRoot found";

		kdb::Key cur;
		kdb::Key root = ks.next();
		if (!root) throw "StructChecker: No root key found";


		while ((cur = ks.next()))
		{
			kdb::Key searchKey = config.next();
			if (!searchKey) throw "StructChecker: More keys found than structure should have";
			if (!cur.isDirectBelow(root)) throw "StructChecker: key is not direct below";

			if (searchKey.getBaseName() != cur.getBaseName())
				throw "StructChecker: did not find expected subkey";

			cur.copyAllMeta (searchKey);
		}

		if (config.next()) throw "StructChecker: There should be more elements in the structure";
	}
};

class ListChecker : public Checker
{
	CheckerPtr structure;

public:
	void buildup (Factory &f, std::string const& templateParameter) override;

	void check (kdb::KeySet &ks) override
	{
		kdb::Key k;
		kdb::KeySet ks2 (ks.dup());

		ks2.rewind();
		kdb::Key root = ks2.next();
		if (!root) throw "ListChecker: no root key found";

		while ((k = ks2.next()))
		{
			if (!root.isDirectBelow(k)) throw "ListChecker: key is not direct below";

			kdb::KeySet cks(ks2.cut(k));

			structure->check(cks);
		}
	}
};

} // end namespace elektra

#endif
