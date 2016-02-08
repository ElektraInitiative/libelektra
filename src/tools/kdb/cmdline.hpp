/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef CMDLINE_HPP
#define CMDLINE_HPP

/* Cmdline parser.
 *
 * To add an option there are 5 steps.
 * Beware not to introduce options which already have
 * an meaning in one of the utilities.
 * Please always append the options in alphabetical order
 * with capitals later.
 */


#include <string>
#include <vector>
#include <map>

class Command;

namespace kdb
{
	class Key;
	class KeySet;
}

class Cmdline
{
public:
	Cmdline (int argc, char **argv, Command *command);
	~Cmdline () {}

	/** The synopsis of the command
	  * Currently it is only printed out.
	  * May be used to determine number of
	  * commands in the future.
	 */
	std::string synopsis;

	/** The help text to printed out. */
	std::string helpText;

	/** At least one of the options was invalid */
	bool invalidOpt;

	/*XXX: Step 1: add your option here.
	 * (please sort by getopt short name, small letters first)*/
	bool debug; /*!< To debug the commands. */
	bool force; /*!< Force the action. */
	bool load; /*!< Load plugins instead of using system/elektra. */
	bool humanReadable; /*!< Human readable values are preferred. */
	bool help; /*!< Display help instead of the normal action.. */
	bool interactive; /*!< Interactive mode. */
	bool noNewline; /*!< Do not output a newline at the end. */
	bool test; /*!< Run some self tests instead of the normal action. */
	bool recursive; /*!< Recursive mode. */
	std::string resolver;
	std::string strategy; /*!< A comma separated list of the used merging strategies. Their order is relevant. */
	bool verbose; /*!< Be more verbose. */
	bool version; /*!< Return version info instead of the normal action.. */
	bool withoutElektra;
	bool null;
	bool first;
	bool second;
	bool third;
	bool withRecommends;
	bool all; /*!< Consider all keys for lookup */
	std::string format;
	std::string plugins;
	std::string pluginsConfig;
	std::string ns;
	std::string editor;

	typedef std::map<std::string, std::string> map;
	map bookmarks;

	kdb::Key createKey(int pos) const;

	kdb::KeySet getPluginsConfig(std::string basepath="user/") const;

	/** The path to the kdb exectuable. */
	std::string executable;

	/** The given name for the current command.
	  * This is the second parameter. */
	std::string commandName;

	/** The arguments given on the commandline. */
	std::vector <std::string> arguments;
};

std::ostream & operator<< (std::ostream & os, Cmdline & cl);

#endif
