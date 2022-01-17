/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <validate.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <kdbio.hpp>
#include <errors/errorFactory.hpp>
#include <iostream>

using namespace std;
using namespace kdb;


/**
 * @brief Validate key database subtree
 *
 * Validate the part of the database that is rooted by the key given in the first argument.
 * This is done by reading all key values, writing a different value, rewriting the original value and then re-setting the values
 * in the key database. All loaded validation plugins are hereby triggered and their warnings are returned.
 *
 * @param cl the command line
 *
 * @return 0 if no warnings or errors were found and validation was therefore successful
 * @return 1 if some warnings and errors occurred and validation therefore failed
 */
int ValidateCommand::execute (Cmdline const & cl)
{
	int argc = cl.arguments.size ();
	if (argc != 1)
	{
		throw invalid_argument ("1 argument needed");
	}

	cout << "The given path was: " << cl.arguments[0] << endl;

	KeySet ksUnfiltered;
	/* use the given cmd line argument as the start key */
	Key root = cl.createKey (0);

	/* if -f (force) was given, the namespace is kept
	 * and check-constraints in the spec:/ namespace
	 * are not considered --> the key can be set to a
	 * value that does not pass the validation criteria,
	 * otherwise a cascading key is created for the
	 * following kdb.get() */
	// Key parentKey = cl.getParentKey (root);

	// do not resume on any get errors
	// otherwise the user might break
	// the config
	kdb.get (ksUnfiltered, root);

	stringstream streamWarnings;
	//printWarnings (streamWarnings, root, cl.verbose, cl.debug);

	string strWarnings = streamWarnings.str ();
	if (strWarnings.empty ()) //TODO: currently not working (always as with -f) because of disabled printWarnings above
	{
		cout << getFormattedSuccessString ("No warnings were issued! :)") << endl;
	}
	else
	{
		cerr << strWarnings;
		root.clear ();

		if (cl.force)
		{
			cout << getFormattedInfoString (
					"Because -f was given, we now try to set the values "
					"despite warnings during getting them...")
			     << endl;

			// TODO: Check how to handle, printWarnings consumed the warnings
			//root = cl.createKey (0);
		}
		else
		{
			cerr << getFormattedErrorString (
					"The validation was stopped because of warnings "
					"while getting the values!")
			     << endl;
			kdb.close ();
			return 1;
		}
	}

	KeySet ksPart (ksUnfiltered.cut (root));

	/* iterate over the result keys */
	for (Key curKey : ksPart)
	{
		/* do lookup (needed for resolving cascading keys) */
		Key lookupKey = ksPart.lookup (curKey);

		if (lookupKey.isBinary ()) continue; // only validate string keys

		/* change value (to enable sync flag */
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
	}catch (KDBException & k) {
		/* TODO: remove test code & refactor to use new c++ classes for whole function (instead of printWarnigns and printError) */
		std::cout << std::endl << "################" << std::endl << "TEST NEW C++ CLASS:" << std::endl;
		tools::errors::Error *err = tools::errors::ErrorFactory::fromKey (root);
		std::cout << *err << std::endl << "INCLUDED WARNINGS: " << std::endl;
		for (tools::errors::Warning *w : *err)
			std::cout << *w << std::endl << "---------------" << std::endl;
		delete err;
		std::cout << "err deleted!" << std::endl << "################" << std::endl << std::endl;
	}

	printWarnings (streamWarnings, root, cl.verbose, cl.debug);
	printError (cerr, root, cl.verbose, cl.debug);

	kdb.close ();
	return 0;
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
