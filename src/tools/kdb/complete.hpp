/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef COMPLETE_H
#define COMPLETE_H

#include <functional>
#include <map>

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class CompleteCommand : public Command
{

public:
	CompleteCommand ();
	~CompleteCommand ();

	virtual std::string getShortOptions () override
	{
		return "mM0";
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

	virtual int execute (Cmdline const & cmdline) override;

private:
	void complete (std::string const & argument, Cmdline const & cmdLine);
	void completeNormal (std::string const & argument, kdb::Key const & parsedArgument, Cmdline const & cmdLine);

	const std::map<kdb::Key, std::pair<int, int>> analyze (const kdb::KeySet & ks, Cmdline const & cmdLine);
	void printResults (
		kdb::Key const & root, const int minDepth, const int maxDepth, Cmdline const & cmdLine,
		std::map<kdb::Key, std::pair<int, int>> const & result,
		std::function<bool (std::pair<kdb::Key, std::pair<int, int>> const & current)> const & filterPredicate,
		std::function<void (std::pair<kdb::Key, std::pair<int, int>> const & current, const bool verbose)> const & resultFilter);

	// helper functions
	int getKeyDepth (kdb::Key const & key);
	const kdb::Key getParentKey (kdb::Key const & key);
	kdb::KeySet getKeys (kdb::Key root, bool cutAtRoot, Cmdline const & cl);
	bool shallShowNextLevel (std::string const & argument);

	void addMountpoints (kdb::KeySet & ks, kdb::Key const & root, Cmdline const & cl);
	void addNamespaces (std::map<kdb::Key, std::pair<int, int>> & hierarchy, Cmdline const & cl);
	void increaseCount (std::map<kdb::Key, std::pair<int, int>> & hierarchy, kdb::Key const & key,
			    std::function<int (int)> const & depthIncreaser);

	// print functions
	static void printBookmarkResult (std::pair<kdb::Key, std::pair<int, int>> const & current, const bool verbose);
	static void printResult (std::pair<kdb::Key, std::pair<int, int>> const & current, const bool verbose);

	// filter functions
	static bool filterDepth (const int minDepth, const int maxDepth, std::pair<kdb::Key, std::pair<int, int>> const & current);
	static bool filterCascading (std::string const & argument, std::pair<kdb::Key, std::pair<int, int>> const & current);
	static bool filterName (std::string const & argument, std::pair<kdb::Key, std::pair<int, int>> const & current);
	static bool filterBookmarks (std::string const & bookmarkName, std::pair<kdb::Key, std::pair<int, int>> const & current);
};

#endif
