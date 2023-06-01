/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef PRINTVISITOR_HPP
#define PRINTVISITOR_HPP

#include "./visitor.hpp"

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
	void visit (ConfigNode & node) override;

	/**
	 * @brief visit
	 * @param model
	 */
	void visit (TreeViewModel * model) override;
};

#endif // PRINTVISITOR_HPP
