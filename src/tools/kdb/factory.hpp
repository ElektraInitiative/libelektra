/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef FACTORY_HPP
#define FACTORY_HPP

#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

#include "ansicolors.hpp"
#include "coloredkdbio.hpp"

#include <command.hpp>
#include <external.hpp>

// TODO: to add a new command, 1.) include your header here
#include <check.hpp>
#include <complete.hpp>
#include <convert.hpp>
#include <cp.hpp>
#include <editor.hpp>
#include <export.hpp>
#include <file.hpp>
#include <find.hpp>
#include <fstab.hpp>
#include <get.hpp>
#include <globalmount.hpp>
#include <globalumount.hpp>
#include <import.hpp>
#include <info.hpp>
#include <list.hpp>
#include <listcommands.hpp>
#include <ls.hpp>
#include <merge.hpp>
#include <metaget.hpp>
#include <metals.hpp>
#include <metaremove.hpp>
#include <metaset.hpp>
#include <mount.hpp>
#include <mv.hpp>
#include <remount.hpp>
#include <rm.hpp>
#include <set.hpp>
#include <sget.hpp>
#include <shell.hpp>
#include <specmount.hpp>
#include <test.hpp>
#include <umount.hpp>
#include <validation.hpp>

class Instancer
{
public:
	virtual Command * get () = 0;
	virtual ~Instancer ()
	{
	}
};

template <class T>
class Cnstancer : public Instancer
{
	virtual T * get () override
	{
		return new T ();
	}
};

class Factory
{
	std::map<std::string, Instancer *> m_factory;

public:
	Factory () : m_factory ()
	{
		// TODO: to add a new command, 2.) add a line here  -> and you are done
		m_factory.insert (std::make_pair ("get", new Cnstancer<GetCommand> ()));
		m_factory.insert (std::make_pair ("set", new Cnstancer<SetCommand> ()));
		m_factory.insert (std::make_pair ("rm", new Cnstancer<RemoveCommand> ()));
		m_factory.insert (std::make_pair ("ls", new Cnstancer<LsCommand> ()));
		m_factory.insert (std::make_pair ("complete", new Cnstancer<CompleteCommand> ()));
		m_factory.insert (std::make_pair ("cp", new Cnstancer<CpCommand> ()));
		m_factory.insert (std::make_pair ("mv", new Cnstancer<MvCommand> ()));
		m_factory.insert (std::make_pair ("mount", new Cnstancer<MountCommand> ()));
		m_factory.insert (std::make_pair ("remount", new Cnstancer<RemountCommand> ()));
		m_factory.insert (std::make_pair ("shell", new Cnstancer<ShellCommand> ()));
		m_factory.insert (std::make_pair ("find", new Cnstancer<FindCommand> ()));
		m_factory.insert (std::make_pair ("getmeta", new Cnstancer<MetaGetCommand> ()));
		m_factory.insert (std::make_pair ("rmmeta", new Cnstancer<MetaRemoveCommand> ()));
		m_factory.insert (std::make_pair ("setmeta", new Cnstancer<MetaSetCommand> ()));
		m_factory.insert (std::make_pair ("lsmeta", new Cnstancer<MetaLsCommand> ()));
		m_factory.insert (std::make_pair ("info", new Cnstancer<InfoCommand> ()));
		m_factory.insert (std::make_pair ("test", new Cnstancer<TestCommand> ()));
		m_factory.insert (std::make_pair ("check", new Cnstancer<CheckCommand> ()));
		m_factory.insert (std::make_pair ("vset", new Cnstancer<ValidationCommand> ()));
		m_factory.insert (std::make_pair ("fstab", new Cnstancer<FstabCommand> ()));
		m_factory.insert (std::make_pair ("export", new Cnstancer<ExportCommand> ()));
		m_factory.insert (std::make_pair ("import", new Cnstancer<ImportCommand> ()));
		m_factory.insert (std::make_pair ("convert", new Cnstancer<ConvertCommand> ()));
		m_factory.insert (std::make_pair ("umount", new Cnstancer<UmountCommand> ()));
		m_factory.insert (std::make_pair ("file", new Cnstancer<FileCommand> ()));
		m_factory.insert (std::make_pair ("sget", new Cnstancer<ShellGetCommand> ()));
		m_factory.insert (std::make_pair ("merge", new Cnstancer<MergeCommand>));
		m_factory.insert (std::make_pair ("list", new Cnstancer<ListCommand> ()));
		m_factory.insert (std::make_pair ("editor", new Cnstancer<EditorCommand> ()));
		m_factory.insert (std::make_pair ("spec-mount", new Cnstancer<SpecMountCommand> ()));
		m_factory.insert (std::make_pair ("smount", new Cnstancer<SpecMountCommand> ()));
		m_factory.insert (std::make_pair ("global-mount", new Cnstancer<GlobalMountCommand> ()));
		m_factory.insert (std::make_pair ("global-umount", new Cnstancer<GlobalUmountCommand> ()));
		m_factory.insert (std::make_pair ("gmount", new Cnstancer<GlobalMountCommand> ()));
		m_factory.insert (std::make_pair ("gumount", new Cnstancer<GlobalUmountCommand> ()));
		m_factory.insert (std::make_pair ("list-commands", new Cnstancer<ListCommandsCommand> ()));
	}

	~Factory ()
	{
		for (auto & elem : m_factory)
		{
			delete elem.second;
		}
	}

	std::vector<std::string> getPrettyCommands () const
	{
		std::vector<std::string> ret;
		for (auto & elem : m_factory)
		{
			std::string text = getStdColor (ANSI_COLOR::BOLD);
			text += elem.first;
			text += getStdColor (ANSI_COLOR::RESET);
			text += "\t";
			Command * cmd = elem.second->get ();
			text += cmd->getShortHelpText ();
			delete cmd;
			ret.push_back (text);
		}
		ret.push_back (getStdColor (ANSI_COLOR::BOLD) + "help" + getStdColor (ANSI_COLOR::RESET) + "\t" +
			       "View the man page of a tool");
		ret.push_back (getStdColor (ANSI_COLOR::BOLD) + "list-tools" + getStdColor (ANSI_COLOR::RESET) + "\t" +
			       "List all external tools");
		return ret;
	}

	/**Returns a list of available commands */
	std::vector<std::string> getCommands () const
	{
		std::vector<std::string> ret;
		for (auto & elem : m_factory)
		{
			ret.push_back (elem.first);
		}
		ret.push_back ("help");
		ret.push_back ("list-tools");
		return ret;
	}

	CommandPtr get (std::string const & which)
	{
		Instancer * instancer = m_factory[which];
		if (instancer)
		{
			CommandPtr ret (instancer->get ());
			return ret;
		}
		else
		{
			m_factory.erase (which);
			return CommandPtr (new ExternalCommand ());
		}
	}
};


#endif
