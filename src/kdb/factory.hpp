#ifndef FACTORY_HPP
#define FACTORY_HPP

#include <map>
#include <string>
#include <memory>
#include <stdexcept>

#include <command.hpp>

//TODO: to add a new command, 1.) include your header here
#include <hello.hpp>
#include <get.hpp>
#include <set.hpp>
#include <ls.hpp>
#include <mount.hpp>
#include <meta.hpp>
#include <info.hpp>
#include <rm.hpp>
#include <shell.hpp>
#include <test.hpp>
#include <check.hpp>
#include <validation.hpp>
#include <cp.hpp>
#include <fstab.hpp>
#include <metals.hpp>

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
		// TODO: to add a new command, 2.) add a line here  -> and you are done
		m_factory.insert(std::make_pair("hello", new Cnstancer<HelloCommand>()));
		m_factory.insert(std::make_pair("get", new Cnstancer<GetCommand>()));
		m_factory.insert(std::make_pair("set", new Cnstancer<SetCommand>()));
		m_factory.insert(std::make_pair("ls", new Cnstancer<LsCommand>()));
		m_factory.insert(std::make_pair("mount", new Cnstancer<MountCommand>()));
		m_factory.insert(std::make_pair("meta-get", new Cnstancer<MetaCommand>()));
		m_factory.insert(std::make_pair("meta-set", new Cnstancer<MetaCommand>()));
		m_factory.insert(std::make_pair("info", new Cnstancer<InfoCommand>()));
		m_factory.insert(std::make_pair("rm", new Cnstancer<RemoveCommand>()));
		m_factory.insert(std::make_pair("shell", new Cnstancer<ShellCommand>()));
		m_factory.insert(std::make_pair("test", new Cnstancer<TestCommand>()));
		m_factory.insert(std::make_pair("check", new Cnstancer<CheckCommand>()));
		m_factory.insert(std::make_pair("validation-set", new Cnstancer<ValidationCommand>()));
		m_factory.insert(std::make_pair("cp", new Cnstancer<CpCommand>()));
		m_factory.insert(std::make_pair("fstab-set", new Cnstancer<FstabCommand>()));
		m_factory.insert(std::make_pair("meta-ls", new Cnstancer<MetaLsCommand>()));
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
