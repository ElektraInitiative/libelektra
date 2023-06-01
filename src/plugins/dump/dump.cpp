/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./dump.hpp"

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <unistd.h>

#include <elektra/core/errors.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/logger.h>

using namespace ckdb;

namespace dump
{

int serialize (std::ostream & os, ckdb::Key * parentKey, ckdb::KeySet * ks, bool useFullNames)
{
	os << "kdbOpen 2" << std::endl;

	size_t rootOffset;
	if (useFullNames)
	{
		rootOffset = 0;
	}
	else
	{
		rootOffset = keyGetNameSize (parentKey);
		if (keyName (parentKey)[rootOffset - 2] == '/')
		{
			rootOffset -= 1;
		}
	}

	ckdb::KeySet * metacopies = ksNew (0, KS_END);
	for (elektraCursor cursor = 0; cursor < ksGetSize (ks); ++cursor)
	{
		ckdb::Key * cur = ksAtCursor (ks, cursor);

		size_t namesize = keyGetNameSize (cur) - rootOffset;
		if (namesize > 0)
		{
			namesize -= 1;
		}

		size_t valuesize = keyGetValueSize (cur);

		bool binary = keyIsBinary (cur) == 1;

		std::string type;
		if (binary)
		{
			type = "binary";
		}
		else
		{
			type = "string";
			valuesize -= 1;
		}

		os << "$key " << type << " " << namesize << " " << valuesize << std::endl;
		if (namesize > 0)
		{
			os << &keyName (cur)[rootOffset];
		}
		os << std::endl;

		if (binary)
		{
			os.write (static_cast<const char *> (keyValue (cur)), valuesize);
			os << std::endl;
		}
		else
		{
			os << keyString (cur) << std::endl;
		}

		ckdb::KeySet * metaKs = keyMeta (cur);
		for (elektraCursor metaCursor = 0; metaCursor < ksGetSize (metaKs); ++metaCursor)
		{
			const ckdb::Key * meta = ksAtCursor (metaKs, metaCursor);

			std::stringstream ss;
			ss << "/" << meta; // use the address of pointer as name

			ckdb::Key * search = keyNew (ss.str ().c_str (), KEY_END);
			ckdb::Key * ret = ksLookup (metacopies, search, 0);

			if (!ret)
			{
				const size_t metaNsOffset = sizeof ("meta:/") - 1;
				/* This metakey was not serialized up to now */
				size_t metanamesize = keyGetNameSize (meta) - 1 - metaNsOffset;
				size_t metavaluesize = keyGetValueSize (meta) - 1;

				os << "$meta " << metanamesize << " " << metavaluesize << std::endl;
				os << keyName (meta) + metaNsOffset << std::endl;
				os << keyString (meta) << std::endl;

				std::stringstream ssv;
				ssv << namesize << " " << metanamesize << std::endl;
				if (namesize > 0)
				{
					ssv << &keyName (cur)[rootOffset];
				}
				ssv << std::endl;
				ssv << keyName (meta) + metaNsOffset << std::endl;
				keySetString (search, ssv.str ().c_str ());

				ksAppendKey (metacopies, search);
			}
			else
			{
				/* Meta key already serialized, write out a reference to it */
				keyDel (search);

				os << "$copymeta " << keyString (ret);
			}
		}

		// flush after every key to speed up streaming
		os.flush ();
	}
	ksDel (metacopies);

	os << "$end" << std::endl;

	return 1;
}

static int decodeLine (std::istream & is, ckdb::Key * parentKey, ckdb::KeySet * ks, std::string & line, ckdb::Key ** curPtr)
{
	ckdb::Key * cur = *curPtr;

	std::vector<char> namebuffer (4048);
	std::vector<char> valuebuffer (4048);
	std::string command;
	size_t nrKeys;
	size_t namesize;
	size_t valuesize;

	std::stringstream ss (line);
	ss >> command;

	if (command == "kdbOpen")
	{
		std::string version;
		ss >> version;
		if (version != "1")
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Wrong version detected in dumpfile: %s", version.c_str ());
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
		ss >> namesize;
		ss >> valuesize;

		if (namesize > namebuffer.size ()) namebuffer.resize (namesize + 1);
		is.read (&namebuffer[0], namesize);
		namebuffer[namesize] = 0;

		std::string name (namebuffer.data ());
		size_t slashIndex = name.find ('/');
		if (slashIndex > 0)
		{
			name = name.substr (0, slashIndex) + ":" + name.substr (slashIndex);
		}

		cur = ckdb::keyNew (name.c_str (), KEY_END);

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

		std::string name (namebuffer.data ());
		size_t slashIndex = name.find ('/');
		if (slashIndex > 0)
		{
			name = name.substr (0, slashIndex) + ":" + name.substr (slashIndex);
		}

		ckdb::Key * search = ckdb::ksLookupByName (ks, name.c_str (), 0);
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
		return 1;
	}
	else
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
			parentKey,
			"Unknown command detected in dumpfile: %s.\nMaybe the file is not in dump configuration file format? "
			"Try to remount with another plugin (eg. ini, ni, etc.)",
			command.c_str ());
		return -1;
	}

	*curPtr = cur;

	return 0;
}

int unserializeVersion1 (std::istream & is, ckdb::Key * parentKey, ckdb::KeySet * ks, const std::string & firstLine)
{
	ckdb::Key * cur = nullptr;
	std::string line = firstLine;

	do
	{
		int ret = decodeLine (is, parentKey, ks, line, &cur);

		if (ret == -1)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (ret == 1)
		{
			break;
		}
	} while (std::getline (is, line));

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int unserializeVersion2 (std::istream & is, ckdb::Key * parentKey, ckdb::KeySet * ks, bool useFullNames)
{
	ckdb::Key * cur = nullptr;
	std::string line;

	std::string rootName (keyName (parentKey));
	rootName += "/";

	if (useFullNames)
	{
		rootName = "";
	}

	char newline;

	while (std::getline (is, line))
	{
		std::stringstream ss (line);
		std::string command;
		ss >> command;

		if (command == "$key")
		{
			std::string type;
			size_t nameSize;
			size_t valueSize;

			ss >> type;
			ss >> nameSize;
			ss >> valueSize;

			std::string name (nameSize, '\0');
			is.read (&name[0], nameSize);

			is.read (&newline, 1);
			if (newline != '\n')
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
									 "Expected newline '\\n' but got '%c' at position %zd.\n", newline,
									 static_cast<size_t> (is.tellg ()));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			if (type == "string")
			{
				std::string value (valueSize, '\0');
				is.read (&value[0], valueSize);

				is.read (&newline, 1);
				if (newline != '\n')
				{
					ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
										 "Expected newline '\\n' but got '%c' at position %zd.\n",
										 newline, static_cast<size_t> (is.tellg ()));
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				cur = keyNew ((rootName + name).c_str (), KEY_VALUE, value.c_str (), KEY_END);
			}
			else if (type == "binary")
			{
				std::vector<char> value (valueSize);
				is.read (value.data (), valueSize);

				is.read (&newline, 1);
				if (newline != '\n')
				{
					ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
										 "Expected newline '\\n' but got '%c' at position %zd.\n",
										 newline, static_cast<size_t> (is.tellg ()));
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (valueSize > 0)
				{
					cur = keyNew ((rootName + name).c_str (), KEY_BINARY, KEY_SIZE, valueSize, KEY_VALUE, value.data (),
						      KEY_END);
				}
				else
				{
					cur = keyNew ((rootName + name).c_str (), KEY_BINARY, KEY_SIZE, valueSize, KEY_END);
				}
			}
			else
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unknown key type detected in dumpfile: %s.\n",
									 type.c_str ());
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			ksAppendKey (ks, cur);
		}
		else if (command == "$meta")
		{
			size_t nameSize;
			size_t valueSize;

			ss >> nameSize;
			ss >> valueSize;

			std::string name (nameSize, '\0');
			is.read (&name[0], nameSize);

			is.read (&newline, 1);
			if (newline != '\n')
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
									 "Expected newline '\\n' but got '%c' at position %zd.\n", newline,
									 static_cast<size_t> (is.tellg ()));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			std::string value (valueSize, '\0');
			is.read (&value[0], valueSize);

			is.read (&newline, 1);
			if (newline != '\n')
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
									 "Expected newline '\\n' but got '%c' at position %zd.\n", newline,
									 static_cast<size_t> (is.tellg ()));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			keySetMeta (cur, name.c_str (), value.c_str ());
		}
		else if (command == "$copymeta")
		{
			size_t keyNameSize;
			size_t metaNameSize;

			ss >> keyNameSize;
			ss >> metaNameSize;

			std::string keyName (keyNameSize, '\0');
			is.read (&keyName[0], keyNameSize);

			is.read (&newline, 1);
			if (newline != '\n')
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
									 "Expected newline '\\n' but got '%c' at position %zd.\n", newline,
									 static_cast<size_t> (is.tellg ()));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			std::string metaName (metaNameSize, '\0');
			is.read (&metaName[0], metaNameSize);

			is.read (&newline, 1);
			if (newline != '\n')
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
									 "Expected newline '\\n' but got '%c' at position %zd.\n", newline,
									 static_cast<size_t> (is.tellg ()));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			ckdb::Key * source = ckdb::ksLookupByName (ks, (rootName + keyName).c_str (), 0);
			keyCopyMeta (cur, source, metaName.c_str ());
		}
		else if (command == "$end")
		{
			break;
		}
		else
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				parentKey,
				"Unknown command detected in dumpfile: %s.\nMaybe the file is not in dump configuration file format? "
				"Try to remount with another plugin (eg. ini, ni, etc.)",
				command.c_str ());
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int unserialize (std::istream & is, ckdb::Key * parentKey, ckdb::KeySet * ks, bool useFullNames = false)
{
	std::string line;

	if (std::getline (is, line))
	{
		if (line == "kdbOpen 2")
		{
			return unserializeVersion2 (is, parentKey, ks, useFullNames);
		}

		return unserializeVersion1 (is, parentKey, ks, line);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief An implementation of std::streambuf that is based on `FILE *` from the C API.
 *
 * This class allows us to read from and write to a `FILE *` via the std::iostream APIs.
 */
class FileStreamBuf : public std::streambuf
{
public:
	FileStreamBuf (FILE * file) : file_ (file)
	{
	}

protected:
	std::streamsize xsputn (const char_type * s, std::streamsize n) override
	{
		int r = fwrite (s, 1, n, file_);
		fflush (file_);
		return r;
	};

	int_type overflow (int_type ch) override
	{
		int r = fwrite (&ch, 1, 1, file_);
		fflush (file_);
		return r;
	}

	int_type underflow () override
	{
		int c = fgetc (file_);
		if (c == EOF)
		{
			this->setg (nullptr, nullptr, nullptr);
		}
		else
		{
			buf_ = c;
			this->setg (&buf_, &buf_, &buf_ + 1);
		}
		return this->gptr () == this->egptr () ? std::char_traits<char>::eof () :
							 std::char_traits<char>::to_int_type (*this->gptr ());
	}

private:
	FILE * file_;
	char buf_;
};

int funserialize (KeySet * ks, FILE * file, Key * errorKey)
{
	FileStreamBuf buf (file);
	std::istream is (&buf);
	return unserialize (is, errorKey, ks, true);
}

int fserialize (KeySet * ks, FILE * file, Key * errorKey)
{
	FileStreamBuf buf (file);
	std::ostream os (&buf);
	return serialize (os, errorKey, ks, true);
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

int elektraDumpGet (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	Key * root = ckdb::keyNew ("system:/elektra/modules/dump", KEY_END);
	if (keyCmp (root, parentKey) == 0 || keyIsBelow (root, parentKey) == 1)
	{
		keyDel (root);
		KeySet * n = ksNew (50, keyNew ("system:/elektra/modules/dump", KEY_VALUE, "dump plugin waits for your orders", KEY_END),
				    keyNew ("system:/elektra/modules/dump/exports", KEY_END),
				    keyNew ("system:/elektra/modules/dump/exports/get", KEY_FUNC, elektraDumpGet, KEY_END),
				    keyNew ("system:/elektra/modules/dump/exports/set", KEY_FUNC, elektraDumpSet, KEY_END),
				    keyNew ("system:/elektra/modules/dump/exports/serialize", KEY_FUNC, dump::serialize, KEY_END),
				    keyNew ("system:/elektra/modules/dump/exports/unserialize", KEY_FUNC, dump::unserialize, KEY_END),
				    keyNew ("system:/elektra/modules/dump/exports/funserialize", KEY_FUNC, dump::funserialize, KEY_END),
				    keyNew ("system:/elektra/modules/dump/exports/fserialize", KEY_FUNC, dump::fserialize, KEY_END),
				    keyNew ("system:/elektra/modules/dump/config/needs/fcrypt/textmode", KEY_VALUE, "0", KEY_END),
#include "./readme_dump.c"
				    keyNew ("system:/elektra/modules/dump/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, n);
		ksDel (n);
		return 1;
	}
	keyDel (root);
	int errnosave = errno;

	// dirty workaround for pluginprocess
	bool useFullNames = ksLookupByName (elektraPluginGetConfig (handle), "/fullname", 0) != NULL;

	// We use dump for the pluginprocess library. Unfortunately on macOS reading from /dev/fd/<fd> via
	// ifstream fails, thus we read directly from unnamed pipes using a custom buffer and read
	const char pipe[] = "/dev/fd/";
	if (!strncmp (keyString (parentKey), pipe, strlen (pipe)))
	{
		int fd = std::stoi (std::string (keyString (parentKey) + strlen (pipe)));
		dump::pipebuf pipebuf (fd);
		std::istream is (&pipebuf);
		return dump::unserialize (is, parentKey, returned, useFullNames);
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

		return dump::unserialize (is, parentKey, returned, useFullNames);
	}
}

int elektraDumpSet (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
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

	// dirty workaround for pluginprocess
	bool useFullNames = ksLookupByName (elektraPluginGetConfig (handle), "/fullname", 0) != NULL;


	return dump::serialize (ofs, parentKey, returned, useFullNames);
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

