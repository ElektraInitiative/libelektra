/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef PRINTVISITOR_HPP
#define PRINTVISITOR_HPP

#include "visitor.hpp"

/**
 * @brief The PrintVisitor class
 */
class PrintVisitor : public Visitor
{
public:
	/**
	 * @brief visit
	 * @param node
	 */
	void visit(ConfigNode& node);

	/**
	 * @brief visit
	 * @param model
	 */
	void visit(TreeViewModel* model);
};

#endif // PRINTVISITOR_HPP
