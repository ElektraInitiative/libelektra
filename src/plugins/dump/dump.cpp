/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "dump.hpp"

using namespace ckdb;

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <unistd.h>

#include <kdberrors.h>
#include <kdblogger.h>

namespace dump
{

int serialise (std::ostream & os, ckdb::Key *, ckdb::KeySet * ks)
{
	ckdb::Key * cur;

	os << "kdbOpen 1" << std::endl;

	os << "ksNew " << ckdb::ksGetSize (ks) << std::endl;

	ckdb::KeySet * metacopies = ckdb::ksNew (0, KS_END);

	ksRewind (ks);
	while ((cur = ksNext (ks)) != nullptr)
	{
		size_t namesize = ckdb::keyGetNameSize (cur);
		size_t valuesize = ckdb::keyGetValueSize (cur);
		os << "keyNew " << namesize << " " << valuesize << std::endl;
		os.write (ckdb::keyName (cur), namesize);
		os.write (static_cast<const char *> (ckdb::keyValue (cur)), valuesize);
		os << std::endl;

		const ckdb::Key * meta;
		ckdb::keyRewindMeta (cur);
		while ((meta = ckdb::keyNextMeta (cur)) != nullptr)
		{
			std::stringstream ss;
			ss << "user/" << meta; // use the address of pointer as name

			ckdb::Key * search = ckdb::keyNew (ss.str ().c_str (), KEY_END);
			ckdb::Key * ret = ksLookup (metacopies, search, 0);

			if (!ret)
			{
				/* This metakey was not serialised up to now */
				size_t metanamesize = ckdb::keyGetNameSize (meta);
				size_t metavaluesize = ckdb::keyGetValueSize (meta);

				os << "keyMeta " << metanamesize << " " << metavaluesize << std::endl;
				os.write (ckdb::keyName (meta), metanamesize);
				os.write (static_cast<const char *> (ckdb::keyValue (meta)), metavaluesize);
				os << std::endl;

				std::stringstream ssv;
				ssv << namesize << " " << metanamesize << std::endl;
				ssv.write (ckdb::keyName (cur), namesize);
				ssv.write (ckdb::keyName (meta), metanamesize);
				ckdb::keySetRaw (search, ssv.str ().c_str (), ssv.str ().size ());

				ksAppendKey (metacopies, search);
			}
			else
			{
				/* Meta key already serialised, write out a reference to it */
				keyDel (search);

				os << "keyCopyMeta ";
				os.write (static_cast<const char *> (ckdb::keyValue (ret)), ckdb::keyGetValueSize (ret));
				os << std::endl;
			}
		}
		os << "keyEnd" << std::endl;
	}
	os << "ksEnd" << std::endl;

	ksDel (metacopies);

	return 1;
}

int unserialise (std::istream & is, ckdb::Key * errorKey, ckdb::KeySet * ks)
{
	ckdb::Key * cur = nullptr;

	std::vector<char> namebuffer (4048);
	std::vector<char> valuebuffer (4048);
	std::string line;
	std::string command;
	size_t nrKeys;
	size_t namesize;
	size_t valuesize;

	while (std::getline (is, line))
	{
		std::stringstream ss (line);
		ss >> command;

		if (command == "kdbOpen")
		{
			std::string version;
			ss >> version;
			if (version != "1")
			{
				ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Wrong version detected in dumpfile: %s", version.c_str ());
				return -1;
			}
		}
		else if (command == "ksNew")
		{
			ss >> nrKeys;

			ksClear (ks);
		}
		else if (command == "keyNew")
		{
			cur = ckdb::keyNew (nullptr);

			ss >> namesize;
			ss >> valuesize;

			if (namesize > namebuffer.size ()) namebuffer.resize (namesize + 1);
			is.read (&namebuffer[0], namesize);
			namebuffer[namesize] = 0;
			ckdb::keySetName (cur, &namebuffer[0]);

			if (valuesize > valuebuffer.size ()) valuebuffer.resize (valuesize + 1);
			is.read (&valuebuffer[0], valuesize);
			valuebuffer[valuesize] = 0;

			ckdb::keySetRaw (cur, &valuebuffer[0], valuesize);
			std::getline (is, line);
		}
		else if (command == "keyMeta")
		{
			ss >> namesize;
			ss >> valuesize;

			if (namesize > namebuffer.size ()) namebuffer.resize (namesize + 1);
			is.read (&namebuffer[0], namesize);
			namebuffer[namesize] = 0;

			if (valuesize > valuebuffer.size ()) valuebuffer.resize (valuesize + 1);
			is.read (&valuebuffer[0], valuesize);
			valuebuffer[valuesize] = 0;

			keySetMeta (cur, &namebuffer[0], &valuebuffer[0]);
			std::getline (is, line);
		}
		else if (command == "keyCopyMeta")
		{
			ss >> namesize;
			ss >> valuesize;

			if (namesize > namebuffer.size ()) namebuffer.resize (namesize + 1);
			is.read (&namebuffer[0], namesize);
			namebuffer[namesize] = 0;

			if (valuesize > valuebuffer.size ()) valuebuffer.resize (valuesize + 1);
			is.read (&valuebuffer[0], valuesize);
			valuebuffer[valuesize] = 0;

			ckdb::Key * search = ckdb::ksLookupByName (ks, &namebuffer[0], 0);
			ckdb::keyCopyMeta (cur, search, &valuebuffer[0]);
			std::getline (is, line);
		}
		else if (command == "keyEnd")
		{
			ckdb::ksAppendKey (ks, cur);
			cur = nullptr;
		}
		else if (command == "ksEnd")
		{
			break;
		}
		else
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				errorKey,
				"Unknown command detected in dumpfile: %s.\nMaybe the file is not in dump configuration file format? "
				"Try to remount with another plugin (eg. ini, ni, etc.)",
				command.c_str ());
			return -1;
		}
	}
	return 1;
}

class pipebuf : public std::streambuf
{
	char * buffer_;
	int fd_;

public:
	pipebuf (int fd) : buffer_ (new char[4096]), fd_ (fd)
	{
	}
	~pipebuf ()
	{
		delete[] this->buffer_;
	}
	int underflow ()
	{
		if (this->gptr () == this->egptr ())
		{
			// read from the pipe directly into the buffer
			ssize_t r = read (fd_, buffer_, 4096);
			this->setg (this->buffer_, this->buffer_, this->buffer_ + r);
		}
		return this->gptr () == this->egptr () ? std::char_traits<char>::eof () :
							 std::char_traits<char>::to_int_type (*this->gptr ());
	}
};

} // namespace dump

extern "C" {

int elektraDumpGet (ckdb::Plugin *, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	Key * root = ckdb::keyNew ("system/elektra/modules/dump", KEY_END);
	if (keyCmp (root, parentKey) == 0 || keyIsBelow (root, parentKey) == 1)
	{
		keyDel (root);
		KeySet * n = ksNew (50, keyNew ("system/elektra/modules/dump", KEY_VALUE, "dump plugin waits for your orders", KEY_END),
				    keyNew ("system/elektra/modules/dump/exports", KEY_END),
				    keyNew ("system/elektra/modules/dump/exports/get", KEY_FUNC, elektraDumpGet, KEY_END),
				    keyNew ("system/elektra/modules/dump/exports/set", KEY_FUNC, elektraDumpSet, KEY_END),
				    keyNew ("system/elektra/modules/dump/exports/serialise", KEY_FUNC, dump::serialise, KEY_END),
				    keyNew ("system/elektra/modules/dump/exports/unserialise", KEY_FUNC, dump::unserialise, KEY_END),
				    keyNew ("system/elektra/modules/dump/config/needs/fcrypt/textmode", KEY_VALUE, "0", KEY_END),
#include "readme_dump.c"
				    keyNew ("system/elektra/modules/dump/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, n);
		ksDel (n);
		return 1;
	}
	keyDel (root);
	int errnosave = errno;

	// We use dump for the pluginprocess library. Unfortunately on macOS reading from /dev/fd/<fd> via
	// ifstream fails, thus we read directly from unnamed pipes using a custom buffer and read
	const char pipe[] = "/dev/fd/";
	if (!strncmp (keyString (parentKey), pipe, strlen (pipe)))
	{
		int fd = std::stoi (std::string (keyString (parentKey) + strlen (pipe)));
		dump::pipebuf pipebuf (fd);
		std::istream is (&pipebuf);
		return dump::unserialise (is, parentKey, returned);
	}
	else
	{
		// ELEKTRA_LOG (ELEKTRA_LOG_MODULE_DUMP, "opening file %s", keyString (parentKey));
		std::ifstream is (keyString (parentKey), std::ios::binary);

		if (!is.is_open ())
		{
			ELEKTRA_SET_ERROR_GET (parentKey);
			errno = errnosave;
			return -1;
		}

		return dump::unserialise (is, parentKey, returned);
	}
}

int elektraDumpSet (ckdb::Plugin *, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	int errnosave = errno;
	// ELEKTRA_LOG (ELEKTRA_LOG_MODULE_DUMP, "opening file %s", keyString (parentKey));
	std::ofstream ofs (keyString (parentKey), std::ios::binary);
	if (!ofs.is_open ())
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	return dump::serialise (ofs, parentKey, returned);
}

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("dump",
		ELEKTRA_PLUGIN_GET,		&elektraDumpGet,
		ELEKTRA_PLUGIN_SET,		&elektraDumpSet,
		ELEKTRA_PLUGIN_END);
}

} // extern C

