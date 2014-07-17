#include "confignode.hpp"

using namespace kdb;

ConfigNode::ConfigNode(const QString& name, const QString& path) :  m_name(name), m_path(path)
{
	KDB kdb;
	KeySet config;
	kdb.get(config, m_path.toStdString());

	m_key = config.lookup(m_path.toStdString());

	if (m_key && m_key.isString())
		m_value = QVariant::fromValue(QString::fromStdString(m_key.getString()));

	else if (m_key && m_key.isBinary())
		m_value = QVariant::fromValue(QString::fromStdString(m_key.getBinary()));
}

ConfigNode::ConfigNode(const ConfigNode& other)
{

}

ConfigNode::ConfigNode()
{
	m_children = QList<ConfigNode*>();
}

ConfigNode::~ConfigNode()
{
	qDeleteAll(m_children);
}

int ConfigNode::getChildCount() const
{
	return m_children.count();
}

QString ConfigNode::getName() const
{
	return m_name;
}

QString ConfigNode::getPath() const
{
	return m_path;
}

QVariant ConfigNode::getValue() const
{
	return m_value;
}

void ConfigNode::setName(const QString& name)
{
	m_name = name;
	emit nameChanged();
}

void ConfigNode::setValue(const QVariant& value)
{
	m_value = value;
}

void ConfigNode::appendChild(ConfigNode* node)
{
	m_children.append(node);
}

bool ConfigNode::hasChild(const QString& name) const
{
	foreach (ConfigNode * node, m_children)
	{
		if (node->getName() == name)
		{
			return true;
		}

	}

	return false;
}

TreeViewModel* ConfigNode::getChildren()
{
	return new TreeViewModel(m_children);
}

TreeViewModel* ConfigNode::getMetaValue()
{

	QList<ConfigNode*> meta;

	if (m_key)
	{
		m_key.rewindMeta();

		while (m_key.nextMeta())
		{
			ConfigNode* node = new ConfigNode();
			node->setName(QString::fromStdString(m_key.currentMeta().getName()));
			node->setValue(QString::fromStdString(m_key.currentMeta().getString()));
			meta << node;
		}
	}

	return new TreeViewModel(meta);
}

ConfigNode* ConfigNode::getChildByName(QString& name)
{
	foreach (ConfigNode * node, m_children)
	{
		if (node->getName() == name)
		{
			return node;
		}
	}

	return new ConfigNode("", "");
}

ConfigNode* ConfigNode::getChildByIndex(int index)
{
	if (index >= 0 && index < m_children.length())
		return m_children.at(index);
	else
		return new ConfigNode("", "");
}

bool ConfigNode::childrenHaveNoChildren() const
{
	int children = 0;

	foreach (ConfigNode * node, m_children)
	{
		children += node->getChildCount();
	}

	return children == 0;
}
