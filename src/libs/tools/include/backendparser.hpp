/**
 * @file
 *
 * @brief Implements ways to parse backends
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef TOOLS_BACKEND_PARSER_HPP
#define TOOLS_BACKEND_PARSER_HPP


#include <algorithm>
#include <initializer_list>
#include <memory>
#include <sstream>
#include <vector>

#include <pluginspec.hpp>

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

kdb::KeySet parsePluginArguments (std::string const & pluginArguments, std::string const & basename = "user:");
PluginSpecVector parseArguments (std::string const & cmdline);
PluginSpecVector parseArguments (std::initializer_list<std::string> cmdline);

namespace detail
{
void processArgument (PluginSpecVector & arguments, size_t & counter, std::string argument);
void fixArguments (PluginSpecVector & arguments);
} // namespace detail

/**
 * @brief Parse a complete commandline that is already tokenized in pluginname pluginconfig
 *
 * @tparam Iterator forward iterator type
 *
 * @param cmdline contains space separated plugins with optional plugin configurations
 *
 * @return a parsed PluginSpecVector
 */
template <typename Iterator>
PluginSpecVector parseArguments (Iterator first, Iterator last)
{
	PluginSpecVector arguments;
	size_t counter = 0;
	while (first != last)
	{
		detail::processArgument (arguments, counter, *first);
		++first;
	}
	detail::fixArguments (arguments);
	return arguments;
}
} // namespace tools
} // namespace kdb

#endif
