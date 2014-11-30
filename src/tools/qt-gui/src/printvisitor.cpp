#include "printvisitor.hpp"

#include "confignode.hpp"
#include "treeviewmodel.hpp"

void PrintVisitor::visit(ConfigNode& node)
{
	QStringList path = node.getPath().split("/");
	QString name;

	foreach (QString s, path)
		name += "> ";

	name += node.getName();

	qDebug() << name;
}

void PrintVisitor::visit(TreeViewModel* model)
{
	foreach (ConfigNodePtr node, model->model())
	{
		node->accept(*this);
	}
}
