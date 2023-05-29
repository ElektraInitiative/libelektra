/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include "./mountOdbc.hpp"
#include <iostream>


MountOdbcCommand::MountOdbcCommand () : kdb (root)
{
}

int MountOdbcCommand::execute (Cmdline const & cl)
{
	std::cout << "This is the new odbc mounting command!" << std::endl;
	std::cout << "For now, a hardcoded mountpoint at user:/odbc is created." << std::endl;

	kdb::Key keyMp ("system:/elektra/mountpoints/user:\\/odbc", KEY_END);
	kdb::Key keyBackend ("system:/elektra/mountpoints/user:\\/odbc/plugins/backend", KEY_END);
	kdb::Key keyBackendName ("system:/elektra/mountpoints/user:\\/odbc/plugins/backend/name", KEY_END);
	kdb::Key keyDataSourceName ("system:/elektra/mountpoints/user:\\/odbc/definition/dataSourceName", KEY_END);
	// kdb::Key keyUserName ("system:/elektra/mountpoints/user:\\/odbc/definition/userName", KEY_END);
	// kdb::Key keyPassword ("system:/elektra/mountpoints/user:\\/odbc/definition/password", KEY_END);
	kdb::Key keyTableName ("system:/elektra/mountpoints/user:\\/odbc/definition/table/name", KEY_END);
	kdb::Key keyKeyColName ("system:/elektra/mountpoints/user:\\/odbc/definition/table/keyColName", KEY_END);
	kdb::Key keyValColName ("system:/elektra/mountpoints/user:\\/odbc/definition/table/valColName", KEY_END);
	kdb::Key keyMetaTableName ("system:/elektra/mountpoints/user:\\/odbc/definition/metaTable/name", KEY_END);
	kdb::Key keyMetaTableKeyColName ("system:/elektra/mountpoints/user:\\/odbc/definition/metaTable/keyColName", KEY_END);
	kdb::Key keyMetaTableMetaKeyColName ("system:/elektra/mountpoints/user:\\/odbc/definition/metaTable/metaKeyColName", KEY_END);
	kdb::Key keyMetaTableMetaValColName ("system:/elektra/mountpoints/user:\\/odbc/definition/metaTable/metaValColName", KEY_END);

	/* SQlite */
	keyBackendName.setString ("backend_odbc");
	keyDataSourceName.setString ("Selektra");
	// keyUserName.setString ("flo");
	// keyPassword.setString ("elektra");
	keyTableName.setString ("elektraKeys");
	keyKeyColName.setString ("keyName");
	keyValColName.setString ("keyValue");
	keyMetaTableName.setString ("metaKeys");
	keyMetaTableKeyColName.setString ("keyName");
	keyMetaTableMetaKeyColName.setString ("metaKeyName");
	keyMetaTableMetaValColName.setString ("metaKeyValue");

	kdb::KeySet ksOdbcConfig;
	ksOdbcConfig.append (keyMp);
	ksOdbcConfig.append (keyBackend);
	ksOdbcConfig.append (keyBackendName);
	ksOdbcConfig.append (keyDataSourceName);
	// ksOdbcConfig.append (keyUserName);
	// ksOdbcConfig.append (keyPassword);
	ksOdbcConfig.append (keyTableName);
	ksOdbcConfig.append (keyKeyColName);
	ksOdbcConfig.append (keyValColName);
	ksOdbcConfig.append (keyMetaTableName);
	ksOdbcConfig.append (keyMetaTableKeyColName);
	ksOdbcConfig.append (keyMetaTableMetaKeyColName);
	ksOdbcConfig.append (keyMetaTableMetaValColName);

	kdb::KeySet ksReturned;

	kdb.get (ksReturned, "system:/elektra/mountpoints");
	ksReturned.append (ksOdbcConfig);
	kdb.set (ksReturned, "system:/elektra/mountpoints");

	return EXIT_SUCCESS;
}

MountOdbcCommand::~MountOdbcCommand () = default;