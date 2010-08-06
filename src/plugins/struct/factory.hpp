#ifndef FACTORY_HPP
#define FACTORY_HPP

#include <map>
#include <string>
#include <memory>
#include <stdexcept>

//TODO: to add a new struct checker, 1.) include your header here
#include "checker.hpp"

namespace elektra {

class Instancer
{
public:
	virtual Checker* get() = 0;
	virtual ~Instancer() {}
};

template <class T>
class Cnstancer: public Instancer
{
	virtual T* get()
	{
		return new T();
	}
};

class StructInstancer: public Instancer
{
	KeySet config;

public:
	StructInstancer (KeySet config) :
		config(config)
	{}

	virtual StructChecker* get()
	{
		return new StructChecker(config);
	}
};

class Factory
{
	std::map<std::string, Instancer*> m_factory;
public:
	Factory(KeySet config) :
		m_factory()
	{
		config.rewind();
		Key root = config.next(); // struct key

		m_factory.insert(std::make_pair("list", new Cnstancer<ListChecker>()));

		Key k;
		while (k = config.next())
		{
			std::cout << "root: " << root.getName() << " k: " << k.getName() << std::endl;
			if (!root.isDirectBelow(k)) throw "Factory: key for configuration is not direct below";

			KeySet cks(config.cut(k));
			std::cout << "creating StructChecker " << k.getBaseName() <<
				" with first key: " << cks.head().getName() << std::endl;
			m_factory.insert(std::make_pair(k.getBaseName(), new StructInstancer(cks)));
		}
	}

	~Factory()
	{
		for (
			std::map<std::string,Instancer*>::iterator it = 
			m_factory.begin();
			it != m_factory.end();
			it++)
		{
			delete it->second;
		}
	}

	std::auto_ptr<Checker> get(std::string const& which)
	{
		Instancer* instancer = m_factory[which];
		if (instancer)
		{
			std::auto_ptr <Checker> ret(instancer->get());
			return ret;
		} else throw "Could not get instance";
	}
};


static inline void doCheck(Checker *c, KeySet ks)
{
	try {
		c->check(ks);
	}
	catch (const char *)
	{
		/* Make sure that it will be released */
		ks.release();
		throw;
	}

	ks.release();
}


static inline Checker* buildChecker(KeySet config)
{
	Key k = config.lookup ("/struct");
	if (!k) throw "No Key describing the struct found";

	Factory f (config.cut(k));

	std::auto_ptr<Checker> c = f.get("list");
	if (!c.get()) throw "Could not create list";

	c->buildup(f, "FStab");
	return c.release();
}

} // end namespace elektra

#endif
