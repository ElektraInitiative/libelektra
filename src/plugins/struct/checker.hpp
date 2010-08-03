#ifndef CHECKER_HPP
#define CHECKER_HPP


#include <map>
#include <string>
#include <sstream>
#include <locale>


namespace elektra {

using namespace kdb;
using namespace std;

class Struct
{
public:
	virtual bool check(KeySet &ks) = 0;
	virtual ~Struct();
};

Struct::~Struct()
{}

class FstabStruct : public Struct
{
public:
	bool check(KeySet &ks)
	{
		ks.rewind();
		Key root = ks.next().dup();
	
		root.addBaseName ("device");
		Key k = ks.lookup(root);
		if (!k) return false;

		root.setBaseName ("mpoint");
		k = ks.lookup(root);
		if (!k) return false;

		root.setBaseName ("type");
		k = ks.lookup(root);
		if (!k) return false;

		root.setBaseName ("options");
		k = ks.lookup(root);
		if (!k) return false;

		root.setBaseName ("dumpfreq");
		k = ks.lookup(root);
		if (!k) return false;

		root.setBaseName ("passno");
		k = ks.lookup(root);
		if (!k) return false;

		std::cout << "Structure check succeeded for " << k.getName() << std::endl;

		return true;
	}
};

class StructChecker
{
	std::map<string, Struct*> structures;

public:
	StructChecker()
	{
		structures.insert (pair<string, Struct*>("FStabEntry", new FstabStruct()));
	}

	bool check (KeySet &ks)
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
			try { whichStruct = k.getMeta<string>("check/struct"); }
			catch (...) { continue; }

			if (structures.find(whichStruct) != structures.end())
			{
				std::cout << "found known structure " << whichStruct << endl;
				/* We found a structure */
				if (!structures[whichStruct]->check(cks)) return false;
				continue;
			}

			std::cout << "We got a struct " << whichStruct << " but could not validate it" << endl;
			return false;
		}
		return true;
	}

	~StructChecker()
	{
		map<string,Struct*>::iterator it;
		for ( it=structures.begin() ; it != structures.end(); it++)
		{
			delete it->second;
		}
	}

};

} // end namespace elektra

#endif
