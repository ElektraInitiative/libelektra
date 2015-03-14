#ifndef KEYSETVISITOR_HPP
#define KEYSETVISITOR_HPP

#include "visitor.hpp"
#include "confignode.hpp"
#include <kdb.hpp>

/**
 * @brief The KeySetVisitor class
 */

class KeySetVisitor : public Visitor
{
public:
	/**
	 * @brief KeySetVisitor
	 */
	explicit KeySetVisitor();

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

	/**
	 * @brief getKeySet
	 * @return
	 */
	kdb::KeySet getKeySet();

private:
	kdb::KeySet m_set;
};

#endif // KEYSETVISITOR_HPP
