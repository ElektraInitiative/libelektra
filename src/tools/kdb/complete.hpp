/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef COMPLETE_H
#define COMPLETE_H

#include <functional>
#include <map>

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class CompleteCommand : public Command
{

public:
	CompleteCommand ();
	~CompleteCommand ();

	virtual std::string getShortOptions () override
	{
		return "dmMv0";
	}

	virtual std::string getSynopsis () override
	{
		return "<path>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Show suggestions how to complete key names.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Suggestions will include existing key names, path segments of existing key names and mountpoints.\n"
		       "Additionally, the output indicates whether the given path is a node or a leaf in the hierarchy of keys.";
	}

	virtual int execute (const Cmdline & cmdline) override;

private:
	void complete (const std::string argument, const Cmdline & cmdLine);
	void completeNormal (const std::string argument, const kdb::Key parsedArgument, const Cmdline & cmdLine);

	const std::map<kdb::Key, std::pair<int, int>> analyze (const kdb::KeySet & ks, const Cmdline & cmdLine);
	void
	printResults (const kdb::Key root, const int minDepth, const int maxDepth, const Cmdline & cmdLine,
		      const std::map<kdb::Key, std::pair<int, int>> & result,
		      const std::function<bool(const std::pair<kdb::Key, std::pair<int, int>> & current)> filterPredicate,
		      const std::function<void(const std::pair<kdb::Key, std::pair<int, int>> & current, const bool verbose)> printResult);

	// helper functions
	const kdb::Key getParentKey (const kdb::Key key);
	kdb::KeySet getKeys (kdb::Key root, bool cutAtRoot);
	bool shallShowNextLevel (const std::string argument);

	void addMountpoints (kdb::KeySet & ks, const kdb::Key root);
	void addNamespaces (std::map<kdb::Key, std::pair<int, int>> & hierarchy, const Cmdline & cl);
	void increaseCount (std::map<kdb::Key, std::pair<int, int>> & hierarchy, const kdb::Key key,
			    const std::function<int(int)> depthIncreaser);

	// print functions
	static void printBookmarkResult (const std::pair<kdb::Key, std::pair<int, int>> & current, const bool verbose);
	static void printResult (const std::pair<kdb::Key, std::pair<int, int>> & current, const bool verbose);

	// filter functions
	static bool filterDepth (const int minDepth, const int maxDepth, const std::pair<kdb::Key, std::pair<int, int>> & current);
	static bool filterCascading (const std::string argument, const std::pair<kdb::Key, std::pair<int, int>> & current);
	static bool filterName (const std::string argument, const std::pair<kdb::Key, std::pair<int, int>> & current);
	static bool filterBookmarks (const std::string bookmarkName, const std::pair<kdb::Key, std::pair<int, int>> & current);
};

#endif
