/**
 * @file
 *
 * @brief Implements a way to read spec for mounting purposes
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_SPEC_READER_HPP
#define TOOLS_SPEC_READER_HPP

#include <kdb.hpp>

#include <pluginspec.hpp>
#include <plugindatabase.hpp>
#include <backendbuilder.hpp>

#include <memory>
#include <unordered_map>

namespace kdb
{

namespace tools
{

class PluginDatabase;

class SpecBackendBuilder : public MountBackendBuilder
{
public:
	explicit SpecBackendBuilder(BackendBuilderInit const & bbi = BackendBuilderInit());
	int nodes;
};

/**
 * @brief Highlevel interface to build a backend from specification.
 */
class SpecReader
{
public: // TODO: make private, TESTING?
	/**
	 * @brief Contains all backends of all found mountpoints
	 */
	std::unordered_map<Key, SpecBackendBuilder> backends;

private:
	/**
	 * @brief Used for crating new BackendBuilder
	 */
	BackendBuilderInit bbi;

public:
	SpecReader();
	explicit SpecReader(BackendBuilderInit const & bbi = BackendBuilderInit());

	~SpecReader();

	/**
	 * @brief Reads in a specification.
	 *
	 * Adds plugins using BackendBuilder during that.
	 *
	 * @param ks
	 */
	void readSpecification (KeySet const & ks);
};

}

}

#endif
