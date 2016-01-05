/**
 * @file
 *
 * @brief Implementation of all exceptions elektratools library might throw
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef TOOLS_EXCEPTION_HPP
#define TOOLS_EXCEPTION_HPP

#include <stdexcept>
#include <memory>

#include <kdbio.hpp>

namespace kdb
{

/**
 * @brief This namespace is for the libtool library.
 *
 * @note You have to link against libelektratools if you want to
 * use functionality from it. Contrary to classes in namespace kdb it
 * is not header-only.
 *
 * @see Backend for an entry point
 */
namespace tools
{

/**
 * @brief All exceptions from the elektratools library are derived from
 * this exception
 */
struct ToolException : public std::runtime_error
{
	ToolException() :
			runtime_error(
		"When you read this, that means there was something wrong with Elektra Tools.\n"
		"Seems like a wrong exception was thrown."
				)
	{};
	ToolException(std::string message) :
			runtime_error(message)
	{};
};

struct ParseException : public ToolException
{
	ParseException(std::string str) :
		m_str(std::move(str))
	{}

	virtual ~ParseException() throw()
	{}

	virtual const char* what() const throw() override
	{
		return m_str.c_str();
	}

	std::string m_str;
};

struct PluginCheckException : public ToolException
{
	virtual const char* what() const throw() override
	{
		return  "When you read this, that means there was something wrong with the plugin.\n"
			"Seems like a check could not specify the error any further";
	}
};

struct BackendCheckException : public ToolException
{
	virtual const char* what() const throw() override
	{
		return  "When you read this, that means there was something wrong with the backend.\n"
			"Seems like a check could not specify the error any further";
	}
};

struct FileNotValidException : public BackendCheckException
{
	virtual const char* what() const throw() override
	{
		return  "The path you entered is invalid.\n"
			"Try to add another path instead.\n"
			"\n"
			"Filenames are typically (depending on\n"
			"resolver) not allowed to contain '..'.\n"
			"\n"
			"For more information see:\n"
			" kdb info <your resolver>\n"
			;
	}
};

struct MountpointInvalidException : public BackendCheckException
{
	virtual const char* what() const throw() override
	{
		return  "Given mountpoint is not a valid keyname, will abort\n"
			"Examples: system/hosts or user/sw/app";
	}
};

struct MountpointAlreadyInUseException : public BackendCheckException
{
	MountpointAlreadyInUseException(std::string str) :
		m_str(std::move(str))
	{}

	virtual ~MountpointAlreadyInUseException() throw()
	{}

	virtual const char* what() const throw() override
	{
		return m_str.c_str();
	}

	std::string m_str;
};

struct NoSuchBackend : public BackendCheckException
{
	explicit NoSuchBackend (std::string const & message) :
		m_str(message)
	{}

	virtual ~NoSuchBackend() throw()
	{}

	virtual const char* what() const throw() override
	{
		return m_str.c_str();
	}
private:
	std::string m_str;
};

struct PluginAlreadyInserted: public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return  "It is not allowed to insert the same plugin again!\n"
			"Try to add other plugins or other refnames (plugin#refname) instead.";
	}
};

struct BadPluginName : public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return  "You entered a bad name for a plugin!\n"
			"A valid name of a plugin is eithern\n"
			"modulename or modulename#label\n"
			"where both modulename and label must start with a-z\n"
			"and then a-z, 0-9 and underscore (_) only";
	}
};

struct TooManyPlugins : public PluginCheckException
{
	TooManyPlugins(std::string str) :
		m_str(std::move(str))
	{}

	virtual ~TooManyPlugins() throw()
	{}

	virtual const char* what() const throw() override
	{
		return m_str.c_str();
	}

	std::string m_str;
};

struct OrderingViolation: public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return  "Ordering Violation!\n"
			"You tried to add a plugin which requests another plugin to be positioned first.\n"
			"Please position the other plugin first and try again.";
	}
};

struct ConflictViolation: public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return  "Conflict Violation!\n"
			"You tried to add a plugin which conflicts with another.\n"
			"Please don't add a plugin which conflicts.";
	}
};


struct NoPlugin : public PluginCheckException
{
	explicit NoPlugin (Key key) :
		m_key(key),
		m_str()
	{}

	explicit NoPlugin (std::string const & message) :
		m_str(message)
	{}

	virtual ~NoPlugin() throw()
	{}

	virtual const char* what() const throw() override
	{
		if (m_str.empty())
		{
			// note that the code will be re-evaluated
			// if it prints nothing, but an expensive
			// function not printing anything seems
			// to be unlikely.
			//
			// note that printError/printWarning will be
			// used either from namespace kdb or global
			// namespace.
			std::stringstream ss;
			ss << "Was not able to load such a plugin!\n\n";
			ss << "Maybe you misspelled it, there is no such plugin or the loader has problems.\n";
			ss << "You might want to try to set LD_LIBRARY_PATH, use kdb-full or kdb-static.\n";
			ss << "Errors/Warnings during loading were:\n";
			printError(ss, m_key);
			printWarnings(ss, m_key);
			m_str = ss.str();
		}
		return m_str.c_str();
	}
private:
	Key m_key;
	mutable std::string m_str;
};

struct ReferenceNotFound: public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return  "Could not find a reference!\n"
			"Seems you forgot to create the reference before using it.\n"
			"Use #modulename#label# before you #ref to it.";
	}
};

struct MissingNeeded : public PluginCheckException
{
	std::string msg;
	MissingNeeded (std::string need)
	{
		msg = std::string(std::string("The plugin ") + need + " is needed by this plugin but it is not provided.");
	}
	~MissingNeeded () throw()
	{}
	virtual const char* what() const throw() override
	{
		return msg.c_str();
	}
};

struct MissingSymbol: public PluginCheckException
{
	std::string msg;
	MissingSymbol (std::string symbol)
	{
		msg = std::string(std::string("The necessary symbol \"") + symbol + "\" is missing in that plugin!");
	}
	~MissingSymbol () throw()
	{}
	virtual const char* what() const throw() override
	{
		return msg.c_str();
	}
};

struct SymbolMismatch: public PluginCheckException
{
	std::string msg;
	SymbolMismatch (std::string symbol)
	{
		msg = std::string(std::string("The symbol \"") + symbol + "\" does not match with other exported information!");
	}
	~SymbolMismatch () throw()
	{}
	virtual const char* what() const throw() override
	{
		return msg.c_str();
	}
};

struct SymbolDuplicate: public PluginCheckException
{
	std::string msg;
	SymbolDuplicate(std::string symbol)
	{
		msg = std::string(std::string("The symbol \"") + symbol + "\" has the same value as another symbol!");
	}
	~SymbolDuplicate () throw()
	{}
	virtual const char* what() const throw() override
	{
		return msg.c_str();
	}
};

struct StoragePlugin : public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return "There need to be exactly one storage plugin!";
	}
};


struct ResolverPlugin : public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return "There need to be exactly one resolver plugin!";
	}
};

struct PluginWrongName : public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return "The real name of the plugin is different!";
	}
};

struct PluginNoContract: public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return "No contract found for that plugin!\n"
			"Make sure you export kdbGet correctly!";
	}
};

struct PluginNoInfo: public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return "No info found for that plugin within contract!";
	}
};

struct VersionInfoMismatch: public PluginCheckException
{
	virtual const char* what() const throw() override
	{
		return "Version info does not match with library!";
	}
};

}

}

#endif
