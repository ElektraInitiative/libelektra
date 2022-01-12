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

#include <iostream>

using namespace std;
using namespace kdb;


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
	printWarnings (streamWarnings, root, cl.verbose, cl.debug);

	string strWarnings = streamWarnings.str ();
	if (strWarnings.empty ())
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


			root = cl.createKey (0); // use a fresh root key for kdb.set, so that no warnings from kdb.get remain
						 // cout << "Now no warnings should be printed: " << endl;
			// printWarnings (cerr, root, cl.verbose, cl.debug);
			// return 0;
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


	// cout << "size of all keys in mount point: " << ksUnfiltered.size () << endl;
	KeySet ksPart (ksUnfiltered.cut (root));
	// cout << "size of requested keys: " << ksPart.size () << endl;

	/* iterate over the result keys */
	// cout << "Iterating over keys:" << endl;
	for (Key curKey : ksPart)
	{
		/* do lookup (needed for resolving cascading keys) */
		Key lookupKey = ksPart.lookup (curKey);

		if (lookupKey.isBinary ()) continue; // only validate string keys

		// cout << "1. curKey name: " << lookupKey.getName() << ", value: " << lookupKey.getString()
		//      << ", sync: " << lookupKey.needSync() << endl;

		/* change value (to enable sync flag */
		lookupKey.setString (lookupKey.getString () + "^");


		// cout << "2. curKey name: " << lookupKey.getName() << ", value: " << lookupKey.getString()
		//      << ", sync: " << lookupKey.needSync() << endl;

		/* change value back to original */
		std::string tmpStr = lookupKey.getString ();

		/* remove last char that was added in the previous step */
		tmpStr.pop_back ();
		lookupKey.setString (tmpStr);

		// cout << "3. curKey name: " << lookupKey.getName() << ", value: " << lookupKey.getString()
		//      << ", sync: " << lookupKey.needSync() << endl;
	}
	/* write back values */
	kdb.set (ksPart, root);

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
