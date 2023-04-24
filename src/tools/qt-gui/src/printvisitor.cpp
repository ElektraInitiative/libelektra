/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./printvisitor.hpp"

#include "./confignode.hpp"
#include "./treeviewmodel.hpp"

void PrintVisitor::visit (ConfigNode & node)
{
	QStringList path = node.getPath ().split ("/");
	QString name;

	foreach (QString s, path)
		name += " ";

	name += node.getName ();

	std::cout << name.toStdString () << std::endl;
}

void PrintVisitor::visit (TreeViewModel * model)
{
	foreach (ConfigNodePtr node, model->model ())
	{
		node->accept (*this);
	}
}
