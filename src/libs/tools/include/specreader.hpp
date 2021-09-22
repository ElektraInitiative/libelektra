/**
 * @file
 *
 * @brief Implements a way to read spec for mounting purposes
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef TOOLS_SPEC_READER_HPP
#define TOOLS_SPEC_READER_HPP

#include <kdb.hpp>

#include <backendbuilder.hpp>
#include <plugindatabase.hpp>
#include <pluginspec.hpp>

#include <memory>
#include <unordered_map>

namespace kdb
{

namespace tools
{

class PluginDatabase;

/**
 * @brief Build individual backend while reading specification
 */
class SpecBackendBuilder : public MountBackendBuilder
{
public:
	explicit SpecBackendBuilder (BackendBuilderInit const & bbi = BackendBuilderInit ());
	int nodes;
};

/**
 * @brief Highlevel interface to build a backend from specification.
 */
class SpecReader
{
public:
	typedef std::unordered_map<Key, SpecBackendBuilder> Backends;

private:
	/**
	 * @brief Contains all backends of all found mountpoints
	 */
	Backends backends;

private:
	/**
	 * @brief Used for crating new BackendBuilder
	 */
	BackendBuilderInit bbi;

public:
	explicit SpecReader (BackendBuilderInit const & bbi = BackendBuilderInit ());

	~SpecReader ();

	/**
	 * @return backends without resolved needs
	 *
	 * @see resolveNeeds()
	 */
	Backends getBackends ()
	{
		return backends;
	}

	/**
	 * @brief Reads in a specification.
	 *
	 * Adds plugins using BackendBuilder during that.
	 *
	 * @param ks
	 */
	void readSpecification (KeySet const & ks);

	/**
	 * Perform some sanity-checks for keys.
	 *
	 * @param key The key to check.
	 * @throws CommandAbortException If a sanity-check fails.
	 */
	void checkKey (const Key key);
};
} // namespace tools
} // namespace kdb

#endif
