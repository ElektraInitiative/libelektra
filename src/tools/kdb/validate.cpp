/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cmdline.hpp>
#include <errors/errorFactory.hpp>
#include <iostream>
#include <kdb.hpp>
#include <mergehelper.hpp> /* for removeNamespace (Key) */
#include <validate.hpp>

using namespace std;
using namespace kdb;


/**
 * @brief Validate key database subtree
 *
 * Validate the part of the database that is rooted by the key given in the first argument.
 * This is done by reading all string key values, writing a different value, rewriting the original value and then re-setting the values
 * in the key database. All loaded validation plugins are hereby triggered and their warnings are returned.
 * Only string keys are validated! Binary keys will be skipped!
 *
 * @param cl the command line
 *
 * @return 0 if no warnings or errors were found and validation was therefore successful
 * @return 11 if some warnings and errors occurred and validation therefore failed
 */
int ValidateCommand::execute (Cmdline const & cl)
{
	int argc = cl.arguments.size ();
	if (argc != 1)
	{
		throw invalid_argument ("1 argument needed");
	}

	KeySet ksUnfiltered;

	/* use the given cmd line argument as the start key */
	Key root = cl.createKey (0);

	if (cl.verbose)
	{
		cout << "The name of the root key is: " + root.getName () << endl;
	}

	/* Remove namespace -> create cascading key, so that
	 * check-constraints in the spec:/ namespace are considered. */
	root = removeNamespace (root);

	// do not resume on any get errors
	// otherwise the user might break
	// the config
	kdb.get (ksUnfiltered, root);

	/* Convert result of kdb.get to Error object of the C++ errors/warnings API */
	tools::errors::Error * result = tools::errors::ErrorFactory::fromKey (root);

	/* If no warnings or errors occurred, the ErrorFactory returns a nullptr. */
	if (result)
	{
		cout << getFormattedInfoString (
				"The following warnings were issued while"
				" trying to get the values of the keys: ")
		     << endl
		     << endl;

		cerr << *result << endl << endl;

		/* After printing the Warnings, the object is no longer needed. */
		delete result;

		if (!cl.force)
		{
			cerr << getFormattedErrorString (
					"The validation was stopped because of warnings "
					"while getting the values!")
			     << endl;
			return 11;
		}
		else if (cl.verbose)
		{
			cout << getFormattedInfoString (
					"Because -f was given, we now try to set the values "
					"despite warnings during getting them...")
			     << endl;
		}
	}
	else
	{
		cout << getFormattedSuccessString ("No warnings were issued! :)") << endl;
	}

	KeySet ksPart (ksUnfiltered.cut (root));

	/* iterate over the result keys */
	for (Key curKey : ksPart)
	{
		/* do lookup (needed for resolving cascading keys) */
		Key lookupKey = ksPart.lookup (curKey);

		/* only validate string keys */
		if (lookupKey.isBinary ()) continue;

		/* change value to enable sync flag */
		lookupKey.setString (lookupKey.getString () + "^");

		/* change value back to original */
		std::string tmpStr = lookupKey.getString ();

		/* remove last char that was added in the previous step */
		tmpStr.pop_back ();
		lookupKey.setString (tmpStr);
	}

	/* write back values */
	try
	{
		kdb.set (ksPart, root);
		return 0;
	}
	catch (KDBException & k)
	{
		cout << getFormattedInfoString ("The following error was issued while trying to set the values back: ") << endl << endl;
		result = tools::errors::ErrorFactory::fromKey (root);
		cerr << *result << endl << endl;
		delete result;
		return 12;
	}
}


std::string ValidateCommand::getFormattedErrorString (const std::string & str)
{
	return getErrorColor (ANSI_COLOR::BOLD) + getErrorColor (ANSI_COLOR::MAGENTA) + str + getErrorColor (ANSI_COLOR::RESET);
}

std::string ValidateCommand::getFormattedSuccessString (const std::string & str)
{
	return getStdColor (ANSI_COLOR::BOLD) + getStdColor (ANSI_COLOR::GREEN) + str + getStdColor (ANSI_COLOR::RESET);
}

std::string ValidateCommand::getFormattedInfoString (const std::string & str)
{
	return getStdColor (ANSI_COLOR::BOLD) + getStdColor (ANSI_COLOR::YELLOW) + str + getStdColor (ANSI_COLOR::RESET);
}
