#ifndef CHECKER_HPP
#define CHECKER_HPP


#include <map>
#include <string>
#include <memory>

#include <key.hpp>
#include <keyset.hpp>


namespace elektra {

using namespace kdb;
using namespace std; // TODO: remove hack!

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

#if __cplusplus > 199711L
typedef std::unique_ptr<Checker> CheckerPtr;
using std::move;
#else
typedef std::auto_ptr<Checker> CheckerPtr;
inline CheckerPtr move(CheckerPtr ptr) {return ptr;}
#endif

class StructChecker : public Checker
{
	KeySet config;
public:
	StructChecker (KeySet config_)
		: config(config_)
	{}

	void buildup (Factory &f, std::string const& templateParameter);

	void check(KeySet &ks)
	{
		config.rewind();

		Key confRoot = config.next();
		if (!confRoot) throw "StructChecker: No confRoot found";

		Key cur;
		Key root = ks.next();
		if (!root) throw "StructChecker: No root key found";


		while (cur = ks.next())
		{
			Key searchKey = config.next();
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
	void buildup (Factory &f, std::string const& templateParameter);

	void check (KeySet &ks)
	{
		Key k;
		KeySet ks2 (ks.dup());

		ks2.rewind();
		Key root = ks2.next();
		if (!root) throw "ListChecker: no root key found";

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
