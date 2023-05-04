/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CMDLINE_HPP
#define CMDLINE_HPP

/* Cmdline parser.
 *
 * To add an option there are 5 steps.
 * Beware not to introduce options which already have
 * a meaning in one of the utilities.
 * Please always append the options in alphabetical order
 * with capitals later.
 */


#include <map>
#include <string>
#include <vector>

#include "coloredkdbio.hpp"

class Command;

namespace kdb
{
class Key;
class KeySet;
} // namespace kdb

class Cmdline
{
public:
	Cmdline (int argc, char ** argv, Command * command);
	~Cmdline ()
	{
	}

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
	bool debug;	    /*!< To debug the commands. */
	bool force;	    /*!< Force the action. */
	bool load;	    /*!< Load plugins instead of using system:/elektra. */
	bool humanReadable; /*!< Human readable values are preferred. */
	bool help;	    /*!< Display help instead of the normal action.. */
	bool interactive;   /*!< Interactive mode. */
	int minDepth;	    /*!< minimum depth for completion suggestions */
	int maxDepth;	    /*!< maximum depth for completion suggestions */
	bool noNewline;	    /*!< Do not output a newline at the end. */
	bool test;	    /*!< Run some self tests instead of the normal action. */
	bool recursive;	    /*!< Recursive mode. */
	std::string resolver;
	std::string strategy; /*!< A comma separated list of the used merging strategies. Their order is relevant. */
	bool verbose;	      /*!< Be more verbose: explain what is happening */
	bool quiet;	      /*!< Be quiet: suppress non-error messages */
	bool version;	      /*!< Return version info instead of the normal action.. */
	bool withoutElektra;
	std::string inputFile;
	bool null;
	bool first;
	bool second;
	bool third;
	bool includeSessionStorage; /*!< Include kbb record session storage */
	bool withRecommends;
	bool all; /*!< Consider all keys for lookup */
	std::string format;
	std::string plugins;
	std::string globalPlugins;
	std::string pluginsConfig;
	std::string color; /*!< colormode "never", "always" and "auto" to print color if output channel is a tty */
	std::string editor;

	typedef std::map<std::string, std::string> map;
	map bookmarks;
	std::string profile;

	kdb::Key createKey (int pos, bool allowCascading = true) const;
	kdb::Key getParentKey (kdb::Key const & key) const;
	kdb::Key resolveBookmark (std::string name) const;

	kdb::KeySet getPluginsConfig (std::string basepath = "user:/") const;

	/** The path to the kdb exectuable. */
	std::string executable;

	/** The given name for the current command.
	 * This is the second parameter. */
	std::string commandName;

	/** The arguments given on the commandline. */
	std::vector<std::string> arguments;
};

std::ostream & operator<< (std::ostream & os, Cmdline & cl);

#endif
