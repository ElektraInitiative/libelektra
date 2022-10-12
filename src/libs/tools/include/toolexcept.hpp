/**
 * @file
 *
 * @brief Implementation of all exceptions elektratools library might throw
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef TOOLS_EXCEPTION_HPP
#define TOOLS_EXCEPTION_HPP

#include <memory>
#include <stdexcept>

#include <kdbio.hpp>

/* SWIG cpp preprocessor is not aware of the override statement */
#ifndef SWIG_WITHOUT_OVERRIDE
#define ELEKTRA_OVERRIDE override
#else
#define ELEKTRA_OVERRIDE
#endif

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
	ToolException ()
	: runtime_error (
		  "When you read this, that means there was something wrong with Elektra Tools.\n"
		  "Seems like a wrong exception was thrown."){};
	explicit ToolException (std::string message) : runtime_error (message){};
};

struct ParseException : public ToolException
{
	explicit ParseException (std::string str) : m_str (std::move (str))
	{
	}

	virtual ~ParseException () throw ()
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return m_str.c_str ();
	}

	std::string m_str;
};

struct PluginCheckException : public ToolException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "When you read this, that means there was something wrong with the plugin.\n"
		       "Seems like a check could not specify the error any further";
	}
};

struct BackendCheckException : public ToolException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "When you read this, that means there was something wrong with the backend.\n"
		       "Seems like a check could not specify the error any further";
	}
};

struct FileNotValidException : public BackendCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "The path you entered is invalid.\n"
		       "Try to add another path instead.\n"
		       "\n"
		       "Filenames are typically (depending on\n"
		       "resolver) not allowed to contain '..'.\n"
		       "\n"
		       "For more information see:\n"
		       " kdb plugin-info <your resolver>";
	}
};

struct MountpointInvalidException : public BackendCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "Given mountpoint is not a valid keyname, will abort\n"
		       "Examples: system:/hosts or user:/sw/app";
	}
};

struct MountpointAlreadyInUseException : public BackendCheckException
{
	explicit MountpointAlreadyInUseException (std::string str) : m_str (std::move (str))
	{
	}

	virtual ~MountpointAlreadyInUseException () throw ()
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return m_str.c_str ();
	}

	std::string m_str;
};

struct NoSuchBackend : public BackendCheckException
{
	explicit NoSuchBackend (std::string const & message) : m_str (message)
	{
	}

	virtual ~NoSuchBackend () throw ()
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return m_str.c_str ();
	}

private:
	std::string m_str;
};

struct PluginAlreadyInserted : public PluginCheckException
{
	explicit PluginAlreadyInserted (std::string name)
	: m_str ("It is not allowed to insert the same plugin (" + name +
		 ") again!\n"
		 "Try to add other plugins or other refnames (part after #) instead.")
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return m_str.c_str ();
	}

	std::string m_str;
};

struct PluginConfigInvalid : public PluginCheckException
{
	explicit PluginConfigInvalid (Key key) : m_key (key), m_str ()
	{
	}

	explicit PluginConfigInvalid (std::string const & message) : m_str (message)
	{
	}

	virtual ~PluginConfigInvalid () throw ()
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		if (m_str.empty ())
		{
			std::stringstream ss;
			ss << "The provided plugin configuration is not valid!\n";
			ss << "Errors/Warnings during the check were:\n";
			printError (ss, m_key, true, true);
			printWarnings (ss, m_key, true, true);
			m_str = ss.str ();
		}
		return m_str.c_str ();
	}

private:
	Key m_key;
	mutable std::string m_str;
};

struct BadPluginName : public PluginCheckException
{
	explicit BadPluginName (std::string name)
	: m_str ("You entered a bad name (" + name +
		 ") for a plugin!\n"
		 "A valid name of a plugin is either\n"
		 "modulename or modulename#refname\n"
		 "where both modulename and refname must start with a-z\n"
		 "and then a-z, 0-9 and underscore (_) only")
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return m_str.c_str ();
	}

	std::string m_str;
};

struct TooManyPlugins : public PluginCheckException
{
	explicit TooManyPlugins (std::string str) : m_str (std::move (str))
	{
	}

	virtual ~TooManyPlugins () throw ()
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return m_str.c_str ();
	}

	std::string m_str;
};

struct OrderingViolation : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "Ordering Violation!\n"
		       "You tried to add a plugin which requests another plugin to be positioned first.\n"
		       "Please position the other plugin first and try again.";
	}
};

struct CyclicOrderingViolation : public OrderingViolation
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "Ordering Violation!\n"
		       "Could not order plugins by their dependency because of cycle.\n"
		       "Either fix plugins or try with other plugins.";
	}
};


struct ConflictViolation : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "Conflict Violation!\n"
		       "You tried to add a plugin which conflicts with another.\n"
		       "Please don't add a plugin which conflicts.";
	}
};


struct NoPlugin : public PluginCheckException
{
	explicit NoPlugin (Key key) : m_key (key), m_str ()
	{
	}

	explicit NoPlugin (std::string const & message) : m_str (message)
	{
	}

	virtual ~NoPlugin () throw ()
	{
	}

	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		if (m_str.empty ())
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
			printError (ss, m_key, true, true);
			printWarnings (ss, m_key, true, true);
			m_str = ss.str ();
		}
		return m_str.c_str ();
	}

private:
	Key m_key;
	mutable std::string m_str;
};

struct ReferenceNotFound : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "Could not find a reference!\n"
		       "Seems you forgot to create the reference before using it.\n"
		       "Use #modulename#label# before you #ref to it.";
	}
};

struct MissingNeeded : public PluginCheckException
{
	std::string msg;
	explicit MissingNeeded (std::string need) : msg ("The plugin " + need + " is needed by this plugin but it is not provided.")
	{
	}
	~MissingNeeded () throw ()
	{
	}
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return msg.c_str ();
	}
};

struct MissingSymbol : public PluginCheckException
{
	std::string msg;
	explicit MissingSymbol (std::string symbol, std::string plugin)
	: msg ("The necessary symbol \"" + symbol + "\" is missing in the plugin \"" + plugin + "\"!")
	{
	}
	~MissingSymbol () throw ()
	{
	}
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return msg.c_str ();
	}
};

struct WrongStatus : public PluginCheckException
{
	std::string msg;
	explicit WrongStatus (std::string status) : msg ("The status \"" + status + "\" is neither a valid enum value nor an integer!")
	{
	}
	~WrongStatus () throw ()
	{
	}
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return msg.c_str ();
	}
};


struct SymbolMismatch : public PluginCheckException
{
	std::string msg;
	explicit SymbolMismatch (std::string symbol) : msg ("The symbol \"" + symbol + "\" does not match with other exported information!")
	{
	}
	~SymbolMismatch () throw ()
	{
	}
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return msg.c_str ();
	}
};

struct NoGlobalPlugin : public PluginCheckException
{
	std::string msg;
	explicit NoGlobalPlugin (std::string plugin) : msg ("The plugin \"" + plugin + "\" is not suitable to be mounted as global plugin!")
	{
	}
	~NoGlobalPlugin () throw ()
	{
	}
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return msg.c_str ();
	}
};


struct SymbolDuplicate : public PluginCheckException
{
	std::string msg;
	explicit SymbolDuplicate (std::string symbol) : msg ("The symbol \"" + symbol + "\" has the same value as another symbol!")
	{
	}
	~SymbolDuplicate () throw ()
	{
	}
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return msg.c_str ();
	}
};

struct StoragePlugin : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "There need to be exactly one storage plugin!";
	}
};


struct ResolverPlugin : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "There need to be exactly one resolver plugin!";
	}
};

struct PluginNoContract : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "No contract found for that plugin!\n"
		       "Make sure you export kdbGet correctly!";
	}
};

struct PluginNoInfo : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "No info found for that plugin within contract!";
	}
};

struct VersionInfoMismatch : public PluginCheckException
{
	virtual const char * what () const throw () ELEKTRA_OVERRIDE
	{
		return "Version info does not match with library!";
	}
};
} // namespace tools
} // namespace kdb

#endif
