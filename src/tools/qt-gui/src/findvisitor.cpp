#include "findvisitor.hpp"
#include "treeviewmodel.hpp"

FindVisitor::FindVisitor(TreeViewModel *searchResults, const QString &term) :
	m_searchResults(searchResults),
	m_term(term)

{
}

void FindVisitor::visit(ConfigNode &node)
{
	if (node.getName().contains(m_term) || node.getValue().toString().contains(m_term))
	{
		m_searchResults->model().append(ConfigNodePtr(&node));
	}

	if(node.getMetaKeys())
	{
		foreach (ConfigNodePtr metaNode, node.getMetaKeys()->model())
		{
			if(metaNode->getName().contains(m_term) || metaNode->getValue().toString().contains(m_term))
			{
				m_searchResults->model().append(ConfigNodePtr(&node));
			}
		}
	}
}

void FindVisitor::visit(TreeViewModel *model)
{
	foreach (ConfigNodePtr node, model->model())
	{
		node->accept(*this);
	}
}
