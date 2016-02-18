/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "keysetvisitor.hpp"
#include "treeviewmodel.hpp"

using namespace kdb;

KeySetVisitor::KeySetVisitor ()
{
}

void KeySetVisitor::visit (ConfigNode & node)
{
	Key key = node.getKey ();

	if (key && key.isValid ())
	{
		m_set.append (key);
	}
}

void KeySetVisitor::visit (TreeViewModel * model)
{
	foreach (ConfigNodePtr node, model->model ())
	{
		node->accept (*this);
	}
}

KeySet KeySetVisitor::getKeySet ()
{
	return m_set.dup ();
}
