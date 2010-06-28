/***************************************************************************
            dump.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Mon May  3 15:22:44 CEST 2010
    copyright            : by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include "dump.hpp"

using namespace ckdb;

namespace dump
{

void serialize(std::ostream &os, ckdb::KeySet *ks)
{
	ckdb::Key *cur;

	os << "ksNew " << ckdb::ksGetSize(ks) << std::endl;

	ckdb::KeySet *metacopies = ckdb::ksNew(0);

	ksRewind(ks);
	while ((cur = ksNext(ks)) != 0)
	{
		size_t namesize = ckdb::keyGetNameSize(cur);
		size_t valuesize = ckdb::keyGetValueSize(cur);
		os << "keyNew " << namesize
		   << " " << valuesize << std::endl;
		os.write(ckdb::keyName(cur), namesize);
		os.write(static_cast<const char*>(ckdb::keyValue(cur)), valuesize);
		os << std::endl;

		const ckdb::Key *meta;
		ckdb::keyRewindMeta(cur);
		while ((meta = ckdb::keyNextMeta(cur)) != 0)
		{
			std::stringstream ss;
			ss << "user/" << meta; // use the address of pointer as name

			ckdb::Key *search = ckdb::keyNew(
					ss.str().c_str(),
					KEY_END);
			ckdb::Key *ret = ksLookup(metacopies, search, 0);

			if (!ret)
			{
				/* This meta key was not serialized up to now */
				size_t metanamesize = ckdb::keyGetNameSize(meta);
				size_t metavaluesize = ckdb::keyGetValueSize(meta);

				os << "keyMeta " << metanamesize
				   << " " << metavaluesize << std::endl;
				os.write (ckdb::keyName(meta), metanamesize);
				os.write (static_cast<const char*>(ckdb::keyValue(meta)), metavaluesize);
				os << std::endl;

				std::stringstream ssv;
				ssv << namesize << " " << metanamesize << std::endl;
				ssv.write(ckdb::keyName(cur), namesize);
				ssv.write (ckdb::keyName(meta), metanamesize);
				ckdb::keySetRaw(search, ssv.str().c_str(), ssv.str().size());

				ksAppendKey(metacopies, search);
			} else {
				/* Meta key already serialized, write out a reference to it */
				keyDel (search);

				os << "keyMetaCopy ";
				os.write(static_cast<const char*>(ckdb::keyValue(ret)), ckdb::keyGetValueSize(ret));
				os << std::endl;
			}
		}
		os << "keyEnd" << std::endl;
	}
	os << "ksEnd" << std::endl;

	ksDel (metacopies);
}

void unserialize(std::istream &is, ckdb::KeySet *ks)
{
	ckdb::Key *cur = 0;

	std::vector<char> namebuffer(4048);
	std::vector<char> valuebuffer(4048);
	std::string line;
	std::string command;
	size_t nrKeys;
	size_t namesize;
	size_t valuesize;

	while(std::getline (is, line))
	{
		std::stringstream ss (line);
		ss >> command;

		if (command == "ksNew")
		{
			ss >> nrKeys;

			ksClear(ks);
		}
		else if (command == "keyNew")
		{
			cur = ckdb::keyNew(0);

			ss >> namesize;
			ss >> valuesize;

			if (namesize > namebuffer.size()) namebuffer.resize(namesize);
			is.read(&namebuffer[0], namesize);
			namebuffer[namesize] = 0;
			ckdb::keySetName(cur, &namebuffer[0]);

			if (valuesize > valuebuffer.size()) valuebuffer.resize(valuesize);
			is.read(&valuebuffer[0], valuesize);
			ckdb::keySetRaw (cur, &valuebuffer[0], valuesize);
			std::getline (is, line);
		}
		else if (command == "keyMeta")
		{
			ss >> namesize;
			ss >> valuesize;

			if (namesize > namebuffer.size()) namebuffer.resize(namesize);
			is.read(&namebuffer[0], namesize);
			namebuffer[namesize] = 0;

			if (valuesize > valuebuffer.size()) valuebuffer.resize(valuesize);
			is.read(&valuebuffer[0], valuesize);

			keySetMeta (cur, &namebuffer[0], &valuebuffer[0]);
			std::getline (is, line);
		}
		else if (command == "keyMetaCopy")
		{
			ss >> namesize;
			ss >> valuesize;

			if (namesize > namebuffer.size()) namebuffer.resize(namesize);
			is.read(&namebuffer[0], namesize);
			namebuffer[namesize] = 0;

			if (valuesize > valuebuffer.size()) valuebuffer.resize(valuesize);
			is.read(&valuebuffer[0], valuesize);

			ckdb::Key * search = ckdb::ksLookupByName(ks, &namebuffer[0], 0);
			ckdb::keyCopyMeta(cur, search, &valuebuffer[0]);
			std::getline (is, line);
		}
		else if (command == "keyEnd")
		{
			ckdb::keyClearSync(cur);
			ckdb::ksAppendKey(ks, cur);
			cur = 0;
		}
		else if (command == "ksEnd")
		{
			break;
		} else {
			std::cerr << "unkown command: " << command << std::endl;
		}
	}
}

} // namespace dump


extern "C" {

ssize_t kdbGet_dump(ckdb::Plugin *, ckdb::KeySet *returned, const ckdb::Key *parentKey)
{
	Key *root = ckdb::keyNew("system/elektra/modules/dump", KEY_END);
	if (keyRel(root, parentKey) >= 0)
	{
		keyDel (root);
		KeySet *info =
			ksNew(50,
			keyNew ("system/elektra/modules/dump",
				KEY_VALUE, "dump plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/dump/exports", KEY_END),
			keyNew ("system/elektra/modules/dump/exports/get",
				KEY_SIZE, sizeof (&kdbGet_dump),
				KEY_BINARY,
				KEY_VALUE, &kdbGet_dump, KEY_END),
			keyNew ("system/elektra/modules/dump/exports/set",
				KEY_SIZE, sizeof (&kdbSet_dump),
				KEY_BINARY,
				KEY_VALUE, &kdbSet_dump, KEY_END),
			keyNew ("system/elektra/modules/dump/exports/serialize",
				KEY_SIZE, sizeof (&dump::serialize),
				KEY_BINARY,
				KEY_VALUE, &dump::serialize, KEY_END),
			keyNew ("system/elektra/modules/dump/exports/unserialize",
				KEY_SIZE, sizeof (&dump::serialize),
				KEY_BINARY,
				KEY_VALUE, &dump::serialize, KEY_END),
			keyNew ("system/elektra/modules/dump/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/dump/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/dump/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/dump/infos/description",
				KEY_VALUE, "Dumps complete Elektra Semantics", KEY_END),
			keyNew ("system/elektra/modules/dump/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/dump/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/dump/infos/version",
				KEY_VALUE, BACKENDVERSION, KEY_END),
			KS_END);
		ksAppend(returned, info);
		ksRewind(returned);

		Key *k;
		while ((k = ksNext(returned)) != 0) keyClearSync(k);
		return ksGetSize(returned);
	}
	keyDel (root);
	std::ifstream ofs(keyString(parentKey));
	if (!ofs.is_open()) return -1;
	dump::unserialize (ofs, returned);

	return ksGetSize(returned); /* success */
}

ssize_t kdbSet_dump(ckdb::Plugin *, ckdb::KeySet *returned, const ckdb::Key *parentKey)
{
	std::ofstream ifs(keyString(parentKey));
	if (!ifs.is_open()) return -1;
	dump::serialize (ifs, returned);

	return ksGetSize(returned);
}

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(dump)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_GET,		&kdbGet_dump,
		ELEKTRA_PLUGIN_SET,		&kdbSet_dump,
		ELEKTRA_PLUGIN_END);
}

} // extern C

