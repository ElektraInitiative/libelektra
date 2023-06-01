/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./findvisitor.hpp"
#include "./treeviewmodel.hpp"

FindVisitor::FindVisitor (TreeViewModel * searchResults, QString term) : m_searchResults (searchResults), m_term (std::move (term))
{
}

void FindVisitor::visit (ConfigNode & node)
{
	bool termFound = false;

	if (node.getPath ().contains (m_term) || node.getValue ().toString ().contains (m_term))
	{
		termFound = true;
	}

	if (node.getMetaKeys () && !termFound)
	{
		foreach (ConfigNodePtr metaNode, node.getMetaKeys ()->model ())
		{
			if (metaNode->getName ().contains (m_term) || metaNode->getValue ().toString ().contains (m_term))
			{
				termFound = true;
				break;
			}
		}
	}

	if (termFound)
		// let the other model delete this node
		m_searchResults->insertRow (m_searchResults->rowCount (), ConfigNodePtr (&node, &ConfigNode::dontDelete), false);
}

void FindVisitor::visit (TreeViewModel * model)
{
	foreach (ConfigNodePtr node, model->model ())
	{
		node->accept (*this);
	}
}
