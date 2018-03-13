/**
 * @file
 *
 * @brief Implementation of InteractiveMergeStrategy
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <helper/keyhelper.hpp>
#include <merging/interactivemergestrategy.hpp>
#include <merging/onesidestrategy.hpp>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

void outputKeyInfo (string whichKey, Key & key, ostream & outputStream)
{
	if (!key)
	{
		outputStream << whichKey << ": does not exist" << endl;
	}
	else
	{
		outputStream << whichKey << " value: " << key.getString () << endl;
	}
}

void InteractiveMergeStrategy::resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result)
{
	ConflictOperation ours = getOurConflictOperation (conflictKey);
	ConflictOperation theirs = getTheirConflictOperation (conflictKey);

	outputStream << "merging key " << conflictKey.getName () << endl;
	outputStream << endl;
	outputStream << "======== CONFLICT ========" << endl;
	outputStream << "our operation: " << MergeConflictOperation::getFromTag (ours) << endl;
	outputStream << "their operation: " << MergeConflictOperation::getFromTag (theirs) << endl;
	outputStream << endl;

	Key baseKey = task.base.lookup (rebasePath (conflictKey, task.mergeRoot, task.baseParent));
	Key ourKey = task.ours.lookup (rebasePath (conflictKey, task.mergeRoot, task.ourParent));
	Key theirKey = task.theirs.lookup (rebasePath (conflictKey, task.mergeRoot, task.theirParent));

	outputStream << "======== KEY VALUES ========" << endl;
	outputKeyInfo ("base", baseKey, outputStream);
	outputKeyInfo ("ours", ourKey, outputStream);
	outputKeyInfo ("theirs", theirKey, outputStream);

	outputStream << endl;

	char choice;

	ConflictResolutionSide side;

	bool repeat;
	string input;
	do
	{
		outputStream << "What do you want to do?" << endl;
		outputStream << "Take [o]urs, [t]eirs, [b]ase, [m]erge meta: ";

		repeat = false;
		getline (inputStream, input);

		if (input.size () == 0 || input.size () > 1)
		{
			repeat = true;
			continue;
		}

		choice = input.at (0);

		switch (choice)
		{
		case 'o':
			side = OURS;
			outputStream << "Choose our key" << endl;
			break;
		case 't':
			side = THEIRS;
			outputStream << "Choose their key" << endl;
			break;
		case 'b':
			side = BASE;
			outputStream << "Choose base key" << endl;
			break;
		default:
			repeat = true;
		}
	} while (repeat);

	outputStream << endl;

	OneSideStrategy strategy (side);
	strategy.resolveConflict (task, conflictKey, result);
	outputStream << "Key merged..." << endl;
}
} // namespace merging
} // namespace tools
} // namespace kdb
