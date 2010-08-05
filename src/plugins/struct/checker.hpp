#ifndef CHECKER_HPP
#define CHECKER_HPP


#include <map>
#include <string>


namespace elektra {

using namespace kdb;
using namespace std;

class Checker
{
public:
	virtual void check(KeySet &ks) = 0;
	virtual ~Checker();
};

Checker::~Checker()
{}

class FstabChecker : public Checker
{
public:
	void check(KeySet &ks)
	{
		ks.rewind();
		Key root = ks.next().dup();
	
		root.addBaseName ("device");
		Key k = ks.lookup(root);
		k.setMeta<std::string>("check/type", "string");
		k.setMeta<std::string>("check/path", "device");
		if (!k) throw "device not found";

		root.setBaseName ("mpoint");
		k = ks.lookup(root);
		k.setMeta<std::string>("check/type", "string");
		k.setMeta<std::string>("check/path", "directory");
		if (!k) throw "mpoint not found";

		root.setBaseName ("type");
		k = ks.lookup(root);
		k.setMeta<std::string>("check/type", "FSType");
		if (!k) throw "type not found";

		root.setBaseName ("options");
		k = ks.lookup(root);
		k.setMeta<std::string>("check/type", "string");
		if (!k) throw "options not found";

		root.setBaseName ("dumpfreq");
		k = ks.lookup(root);
		k.setMeta<std::string>("check/type", "unsigned_short");
		if (!k) throw "dumpfreq not found";

		root.setBaseName ("passno");
		k = ks.lookup(root);
		k.setMeta<std::string>("check/type", "unsigned_short");
		if (!k) throw "passno not found";
	}
};

class ListChecker : public Checker
{
	Checker* structure;

public:
	ListChecker(Checker *which)
	{
		structure = which;
	}

	void check (KeySet &ks)
	{
		Key k;
		KeySet ks2 (ks.dup());

		ks2.rewind();
		Key root = ks2.next();

		while (k = ks2.next())
		{
			if (!root.isDirectBelow(k)) throw "key is not direct below";

			KeySet cks(ks2.cut(k));

			structure->check(cks);
		}
	}

	~ListChecker()
	{
		delete structure;
	}

};

class StructChecker : public Checker
{
	std::map<string, Checker*> structures;

public:
	StructChecker()
	{
		structures.insert (pair<string, Checker*>("FStabEntry",
					new FstabChecker()));
	}

	void check (KeySet &ks)
	{
		Key k;
		KeySet ks2 (ks.dup());

		ks2.rewind();
		ks2.next(); // ignore root key (because it is a list)
		while (k = ks2.next())
		{
			KeySet cks(ks2.cut(k));
			string whichStruct;
			/* TODO: meta interface without exceptions needed */
			try { whichStruct = k.getMeta<string>("check/Checker"); }
			catch (...) { continue; }

			if (structures.find(whichStruct) != structures.end())
			{
				/* We found a structure */
				structures[whichStruct]->check(cks);
				continue;
			}

			throw "did not found correct checker";
		}
	}

	~StructChecker()
	{
		map<string,Checker*>::iterator it;
		for ( it=structures.begin() ; it != structures.end(); it++)
		{
			delete it->second;
		}
	}

};

} // end namespace elektra

#endif
