#ifndef FACTORY_HPP
#define FACTORY_HPP

#include <map>
#include <string>
#include <memory>
#include <stdexcept>

#include <command.hpp>
#include <hello.hpp>

class Instancer
{
public:
	virtual Command* get() = 0;
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

class Factory
{
	std::map<std::string, Instancer*> m_factory;
public:
	Factory() :
		m_factory()
	{
		m_factory.insert(std::make_pair("hello", new Cnstancer<HelloCommand>()));
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

	std::auto_ptr<Command> get(std::string const& which)
	{
		Instancer* instancer = m_factory[which];
		if (instancer)
		{
			std::auto_ptr <Command> ret(instancer->get());
			return ret;
		} else throw std::out_of_range("unknown command");
	}
};



#endif
