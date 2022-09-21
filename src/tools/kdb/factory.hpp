/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef FACTORY_HPP
#define FACTORY_HPP

#include <algorithm>
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
#include <basename.hpp>
#include <cache.hpp>
#include <cmerge.hpp>
#include <complete.hpp>
#include <convert.hpp>
#include <cp.hpp>
#include <dirname.hpp>
#include <editor.hpp>
#include <export.hpp>
#include <file.hpp>
#include <find.hpp>
#include <gen.hpp>
#include <import.hpp>
#include <listcommands.hpp>
#include <ls.hpp>
#include <memory>
#include <merge.hpp>
#include <metaget.hpp>
#include <metals.hpp>
#include <metaremove.hpp>
#include <metaset.hpp>
#include <mount.hpp>
#include <mountOdbc.hpp>
#include <mv.hpp>
#include <namespace.hpp>
#include <plugincheck.hpp>
#include <plugininfo.hpp>
#include <pluginlist.hpp>
#include <recordexport.hpp>
#include <recordreset.hpp>
#include <recordrm.hpp>
#include <recordstart.hpp>
#include <recordstate.hpp>
#include <recordstop.hpp>
#include <recordundo.hpp>
#include <remount.hpp>
#include <rm.hpp>
#include <set.hpp>
#include <sget.hpp>
#include <shell.hpp>
#include <showmeta.hpp>
#include <specmount.hpp>
#include <test.hpp>
#include <umount.hpp>
#include <validate.hpp>

class Instancer
{
public:
	virtual std::unique_ptr<Command> get () = 0;
	virtual ~Instancer ()
	{
	}
};

template <class T>
class Cnstancer : public Instancer
{
	virtual std::unique_ptr<Command> get () override
	{
		return std::unique_ptr<Command> (new T ());
	}
};

class Factory
{
	std::map<std::string, std::shared_ptr<Instancer>> m_factory;

public:
	Factory () : m_factory ()
	{
		// TODO: to add a new command, 2.) add a line here  -> and you are done
		m_factory.insert (std::make_pair ("set", std::make_shared<Cnstancer<SetCommand>> ()));
		m_factory.insert (std::make_pair ("rm", std::make_shared<Cnstancer<RemoveCommand>> ()));
		m_factory.insert (std::make_pair ("ls", std::make_shared<Cnstancer<LsCommand>> ()));
		m_factory.insert (std::make_pair ("cache", std::make_shared<Cnstancer<CacheCommand>> ()));
		m_factory.insert (std::make_pair ("complete", std::make_shared<Cnstancer<CompleteCommand>> ()));
		m_factory.insert (std::make_pair ("cp", std::make_shared<Cnstancer<CpCommand>> ()));
		m_factory.insert (std::make_pair ("mv", std::make_shared<Cnstancer<MvCommand>> ()));
		m_factory.insert (std::make_pair ("mount", std::make_shared<Cnstancer<MountCommand>> ()));
		m_factory.insert (std::make_pair ("remount", std::make_shared<Cnstancer<RemountCommand>> ()));
		m_factory.insert (std::make_pair ("mountOdbc", std::make_shared<Cnstancer<MountOdbcCommand>> ()));
		m_factory.insert (std::make_pair ("shell", std::make_shared<Cnstancer<ShellCommand>> ()));
		m_factory.insert (std::make_pair ("find", std::make_shared<Cnstancer<FindCommand>> ()));
		m_factory.insert (std::make_pair ("meta-get", std::make_shared<Cnstancer<MetaGetCommand>> ()));
		m_factory.insert (std::make_pair ("meta-show", std::make_shared<Cnstancer<ShowMetaCommand>> ()));
		m_factory.insert (std::make_pair ("meta-rm", std::make_shared<Cnstancer<MetaRemoveCommand>> ()));
		m_factory.insert (std::make_pair ("meta-set", std::make_shared<Cnstancer<MetaSetCommand>> ()));
		m_factory.insert (std::make_pair ("meta-ls", std::make_shared<Cnstancer<MetaLsCommand>> ()));
		m_factory.insert (std::make_pair ("plugin-info", std::make_shared<Cnstancer<PluginInfoCommand>> ()));
		m_factory.insert (std::make_pair ("test", std::make_shared<Cnstancer<TestCommand>> ()));
		m_factory.insert (std::make_pair ("plugin-check", std::make_shared<Cnstancer<PluginCheckCommand>> ()));
		m_factory.insert (std::make_pair ("export", std::make_shared<Cnstancer<ExportCommand>> ()));
		m_factory.insert (std::make_pair ("import", std::make_shared<Cnstancer<ImportCommand>> ()));
		m_factory.insert (std::make_pair ("convert", std::make_shared<Cnstancer<ConvertCommand>> ()));
		m_factory.insert (std::make_pair ("umount", std::make_shared<Cnstancer<UmountCommand>> ()));
		m_factory.insert (std::make_pair ("file", std::make_shared<Cnstancer<FileCommand>> ()));
		m_factory.insert (std::make_pair ("sget", std::make_shared<Cnstancer<ShellGetCommand>> ()));
		m_factory.insert (std::make_pair ("merge", std::make_shared<Cnstancer<MergeCommand>> ()));
		m_factory.insert (std::make_pair ("cmerge", std::make_shared<Cnstancer<CMergeCommand>> ()));
		m_factory.insert (std::make_pair ("plugin-list", std::make_shared<Cnstancer<PluginListCommand>> ()));
		m_factory.insert (std::make_pair ("editor", std::make_shared<Cnstancer<EditorCommand>> ()));
		m_factory.insert (std::make_pair ("spec-mount", std::make_shared<Cnstancer<SpecMountCommand>> ()));
		m_factory.insert (std::make_pair ("list-commands", std::make_shared<Cnstancer<ListCommandsCommand>> ()));
		m_factory.insert (std::make_pair ("gen", std::make_shared<Cnstancer<GenCommand>> ()));
		m_factory.insert (std::make_pair ("namespace", std::make_shared<Cnstancer<NamespaceCommand>> ()));
		m_factory.insert (std::make_pair ("basename", std::make_shared<Cnstancer<BasenameCommand>> ()));
		m_factory.insert (std::make_pair ("dirname", std::make_shared<Cnstancer<DirnameCommand>> ()));
		m_factory.insert (std::make_pair ("validate", std::make_shared<Cnstancer<ValidateCommand>> ()));
		m_factory.insert (std::make_pair ("record-reset", std::make_shared<Cnstancer<RecordResetCommand>> ()));
		m_factory.insert (std::make_pair ("record-export", std::make_shared<Cnstancer<RecordExportCommand>> ()));
		m_factory.insert (std::make_pair ("record-rm", std::make_shared<Cnstancer<RecordRemoveKeyCommand>> ()));
		m_factory.insert (std::make_pair ("record-start", std::make_shared<Cnstancer<RecordStartCommand>> ()));
		m_factory.insert (std::make_pair ("record-state", std::make_shared<Cnstancer<RecordStateCommand>> ()));
		m_factory.insert (std::make_pair ("record-stop", std::make_shared<Cnstancer<RecordStopCommand>> ()));
		m_factory.insert (std::make_pair ("record-undo", std::make_shared<Cnstancer<RecordUndoCommand>> ()));
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
			CommandPtr cmd = elem.second->get ();
			text += cmd->getShortHelpText ();
			ret.push_back (text);
		}
		ret.push_back (getStdColor (ANSI_COLOR::BOLD) + "help" + getStdColor (ANSI_COLOR::RESET) + "\t" +
			       "View the man page of a tool");
		ret.push_back (getStdColor (ANSI_COLOR::BOLD) + "list-tools" + getStdColor (ANSI_COLOR::RESET) + "\t" +
			       "List all external tools");
		std::sort (ret.begin (), ret.end ());
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

		std::sort (ret.begin (), ret.end ());
		return ret;
	}

	CommandPtr get (std::string const & which)
	{
		std::shared_ptr<Instancer> instancer = m_factory[which];
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
