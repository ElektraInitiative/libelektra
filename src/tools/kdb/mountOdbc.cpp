/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include "./mountOdbc.hpp"

#include <cmdline.hpp>
#include <iostream>
#include <kdbassert.h>


MountOdbcCommand::MountOdbcCommand () : kdb (root)
{
}
MountOdbcCommand::~MountOdbcCommand () = default;

void MountOdbcCommand::checkArguments (const Cmdline & cl)
{
	if (cl.arguments.size () != 11)
	{
		throw std::invalid_argument ("11 arguments required, but only " + std::to_string (cl.arguments.size ()) + " provided.");
	}
}

void replaceSubstrings (std::string & str, std::string oldSubStr, std::string newSubStr)
{
	for (size_t i = 0; (i = str.find (oldSubStr, i)) != std::string::npos; i += newSubStr.length ())
	{
		str.replace (i, oldSubStr.length (), newSubStr);
	}
}


int MountOdbcCommand::execute (Cmdline const & cl)
{
	checkArguments (cl);
	printWarnings (std::cerr, root, cl.verbose, cl.debug);

	/* throws exception if no valid key was provided */
	kdb::Key keyMpPath = cl.createKey (10);

	std::string mp = keyMpPath.getName ();

	/* escape slashes in the mountpoint path */
	std::string mpOriginal (mp);
	replaceSubstrings (mp, "/", "\\/");

	std::string dataSourceName = cl.arguments[0];
	std::string userName = cl.arguments[1];
	std::string password = cl.arguments[2];
	std::string tableName = cl.arguments[3];
	std::string keyColName = cl.arguments[4];
	std::string valColName = cl.arguments[5];
	std::string metaTableName = cl.arguments[6];
	std::string mtKeyColName = cl.arguments[7];
	std::string mtMetaKeyColName = cl.arguments[8];
	std::string mtMetaValColName = cl.arguments[9];

	if (dataSourceName.empty () || tableName.empty () || keyColName.empty () || valColName.empty () || metaTableName.empty () ||
	    mtKeyColName.empty () || mtMetaKeyColName.empty () || mtMetaValColName.empty ())
	{
		throw std::invalid_argument (
			"Empty strings (\"\") are only allowed for the 'username' and 'password' arguments.\n"
			"For all other arguments, valid non-empty strings must be provided!");
	}

	kdb::KeySet ksOdbcConfig;
	ssize_t requiredSize = 11;

	/* Now use the specified mountpoint to create the mountpoint configuration under "system:/elektra/mountpoints" */
	kdb::Key keyMp ("system:/elektra/mountpoints/" + mp, KEY_END);
	ksOdbcConfig.append (keyMp);
	ksOdbcConfig.append (kdb::Key ("system:/elektra/mountpoints/" + mp + "/plugins/backend", KEY_END));
	ksOdbcConfig.append (kdb::Key ("system:/elektra/mountpoints/" + mp + "/plugins/backend/name", KEY_VALUE, "backend_odbc", KEY_END));
	ksOdbcConfig.append (
		kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/dataSourceName", KEY_VALUE, dataSourceName.c_str (), KEY_END));

	if (!userName.empty ())
	{
		ksOdbcConfig.append (
			kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/userName", KEY_VALUE, userName.c_str (), KEY_END));
		requiredSize++;
	}

	if (!password.empty ())
	{
		ksOdbcConfig.append (
			kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/password", KEY_VALUE, password.c_str (), KEY_END));
		requiredSize++;
	}

	ksOdbcConfig.append (
		kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/table/name", KEY_VALUE, tableName.c_str (), KEY_END));
	ksOdbcConfig.append (
		kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/table/keyColName", KEY_VALUE, keyColName.c_str (), KEY_END));
	ksOdbcConfig.append (
		kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/table/valColName", KEY_VALUE, valColName.c_str (), KEY_END));
	ksOdbcConfig.append (
		kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/metaTable/name", KEY_VALUE, metaTableName.c_str (), KEY_END));
	ksOdbcConfig.append (kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/metaTable/keyColName", KEY_VALUE,
				       mtKeyColName.c_str (), KEY_END));
	ksOdbcConfig.append (kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/metaTable/metaKeyColName", KEY_VALUE,
				       mtMetaKeyColName.c_str (), KEY_END));
	ksOdbcConfig.append (kdb::Key ("system:/elektra/mountpoints/" + mp + "/definition/metaTable/metaValColName", KEY_VALUE,
				       mtMetaValColName.c_str (), KEY_END));


	ELEKTRA_ASSERT (ksOdbcConfig.size () == requiredSize,
			"The KeySet with the configuration for the ODBC mountpoint has %zd keys, but should have %zd keys.",
			ksOdbcConfig.size (), requiredSize);

	kdb::KeySet ksReturned;

	kdb.get (ksReturned, "system:/elektra/mountpoints");

	if (ksReturned.lookup (keyMp))
	{
		std::cerr << "The mountpoint " << mpOriginal
			  << " already exists.\nPlease choose another mountpoint or unmount the currently mounted backend for the "
			     "specified mountpoint before."
			  << std::endl;
		return EXIT_FAILURE;
	}


	ksReturned.append (ksOdbcConfig);
	if (kdb.set (ksReturned, "system:/elektra/mountpoints") == 1)
	{
		std::cout << "This new mountpoint for the ODBC data source was successfully created!" << std::endl;
		return EXIT_SUCCESS;
	}
	else

	{
		return EXIT_FAILURE;
	}
}
