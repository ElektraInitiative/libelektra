/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "elektragen.hpp"

kainjow::mustache::data ElektraGenTemplate::getTemplateData ()
{
	using namespace kainjow::mustache;

	return data{ "name", "Max Mustermann" };
}