/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef FACTORY_HPP
#define FACTORY_HPP

#include <map>
#include <vector>
#include <string>
#include <memory>
#include <stdexcept>

#include <command.hpp>

//TODO: to add a new command, 1.) include your header here
#include <get.hpp>
#include <set.hpp>
#include <ls.hpp>
#include <mount.hpp>
#include <remount.hpp>
#include <metaget.hpp>
#include <metaset.hpp>
#include <metals.hpp>
#include <info.hpp>
#include <rm.hpp>
#include <shell.hpp>
#include <test.hpp>
#include <check.hpp>
#include <validation.hpp>
#include <cp.hpp>
#include <mv.hpp>
#include <fstab.hpp>
#include <export.hpp>
#include <import.hpp>
#include <convert.hpp>
#include <umount.hpp>
#include <file.hpp>
#include <sget.hpp>
#include <merge.hpp>
#include <list.hpp>
#include <editor.hpp>
#include <external.hpp>

class Instancer
{
public:
	virtual Command* get() = 0;
	virtual ~Instancer() {}
};

template <class T>
class Cnstancer: public Instancer
{
	virtual T* get() override
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
		m_factory.insert(std::make_pair("get", new Cnstancer<GetCommand>()));
		m_factory.insert(std::make_pair("set", new Cnstancer<SetCommand>()));
		m_factory.insert(std::make_pair("rm", new Cnstancer<RemoveCommand>()));
		m_factory.insert(std::make_pair("ls", new Cnstancer<LsCommand>()));
		m_factory.insert(std::make_pair("cp", new Cnstancer<CpCommand>()));
		m_factory.insert(std::make_pair("mv", new Cnstancer<MvCommand>()));
		m_factory.insert(std::make_pair("mount", new Cnstancer<MountCommand>()));
		m_factory.insert(std::make_pair("remount", new Cnstancer<RemountCommand>()));
		m_factory.insert(std::make_pair("shell", new Cnstancer<ShellCommand>()));
		m_factory.insert(std::make_pair("getmeta", new Cnstancer<MetaGetCommand>()));
		m_factory.insert(std::make_pair("setmeta", new Cnstancer<MetaSetCommand>()));
		m_factory.insert(std::make_pair("lsmeta", new Cnstancer<MetaLsCommand>()));
		m_factory.insert(std::make_pair("info", new Cnstancer<InfoCommand>()));
		m_factory.insert(std::make_pair("test", new Cnstancer<TestCommand>()));
		m_factory.insert(std::make_pair("check", new Cnstancer<CheckCommand>()));
		m_factory.insert(std::make_pair("vset", new Cnstancer<ValidationCommand>()));
		m_factory.insert(std::make_pair("fstab", new Cnstancer<FstabCommand>()));
		m_factory.insert(std::make_pair("export", new Cnstancer<ExportCommand>()));
		m_factory.insert(std::make_pair("import", new Cnstancer<ImportCommand>()));
		m_factory.insert(std::make_pair("convert", new Cnstancer<ConvertCommand>()));
		m_factory.insert(std::make_pair("umount", new Cnstancer<UmountCommand>()));
		m_factory.insert(std::make_pair("file", new Cnstancer<FileCommand>()));
		m_factory.insert(std::make_pair("sget", new Cnstancer<ShellGetCommand>()));
		m_factory.insert(std::make_pair("merge", new Cnstancer<MergeCommand>));
		m_factory.insert(std::make_pair("list", new Cnstancer<ListCommand>()));
		m_factory.insert(std::make_pair("editor", new Cnstancer<EditorCommand>()));
	}

	~Factory()
	{
		for (auto & elem : m_factory)
		{
			delete elem.second;
		}
	}

	/**Returns a list of available commands */
	std::vector<std::string> getCommands() const
	{
		std::vector<std::string> ret;
		for (auto & elem : m_factory)
		{
			std::string text = elem.first;
			text+= "\t";
			Command * cmd = elem.second->get();
			text+= cmd->getShortHelpText();
			delete cmd;
			ret.push_back(text);
		}
		return ret;
	}

	CommandPtr get(std::string const& which)
	{
		Instancer* instancer = m_factory[which];
		if (instancer)
		{
			CommandPtr ret(instancer->get());
			return ret;
		}
		else
		{
			m_factory.erase(which);
			return CommandPtr(new ExternalCommand());
		}

	}
};



#endif
