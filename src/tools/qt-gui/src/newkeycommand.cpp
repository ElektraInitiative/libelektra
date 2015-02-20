#include "newkeycommand.hpp"

NewKeyCommand::NewKeyCommand(ConfigNodePtr parentNode, QString path, const QString& value, const QVariantMap& metaData, bool isBelow, QUndoCommand* parent)
	: QUndoCommand(parent)
	, m_parentNode(parentNode)
	, m_newNode(NULL)
	, m_name(path.split(qApp->property("KEY_DELIMITER").toRegularExpression()).at(0))
	, m_value(value)
	, m_metaData(metaData)
{
	if(path.split(qApp->property("KEY_DELIMITER").toRegularExpression()).count() > 1 || isBelow)
		setText("newBranch");
	else
		setText("newKey");

	kdb::Key newKey = m_parentNode->getChildren()->createNewKey(m_parentNode->getPath() + "/" + path, m_value, m_metaData);

	m_parentNode->getChildren()->sink(m_parentNode, path.split(qApp->property("KEY_DELIMITER").toRegularExpression()), newKey.dup());
	m_newNode = m_parentNode->getChildByName(m_name);
	m_parentNode->getChildren()->removeRow(m_parentNode->getChildIndexByName(m_name));
}

void NewKeyCommand::undo()
{
	//remove new node
	m_parentNode->getChildren()->removeRow(m_parentNode->getChildIndexByName(m_name));
}

void NewKeyCommand::redo()
{
	//insert new node
	m_parentNode->getChildren()->append(m_newNode);
}
