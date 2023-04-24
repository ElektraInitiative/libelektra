/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KEYSETVISITOR_HPP
#define KEYSETVISITOR_HPP

#include "./confignode.hpp"
#include "./visitor.hpp"
#include <kdb.hpp>

/**
 * @brief The KeySetVisitor class. It visits every existing ConfigNode and collects the encapsuled key, if it exists.
 */

class KeySetVisitor : public Visitor
{
public:
	/**
	 * @brief KeySetVisitor The default constructor.
	 */
	explicit KeySetVisitor ();

	void visit (ConfigNode & node) override;
	void visit (TreeViewModel * model) override;

	/**
	 * @brief getKeySet Returns the kdb::KeySet with all current valid keys
	 * @return The kdb::KeySet with all current valid keys
	 */
	kdb::KeySet getKeySet ();

private:
	kdb::KeySet m_set;
};

#endif // KEYSETVISITOR_HPP
