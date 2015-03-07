#include "newkeycommand.hpp"

NewKeyCommand::NewKeyCommand(ConfigNodePtr parentNode, DataContainer *data, bool isBelow, QUndoCommand* parent)
	: QUndoCommand(parent)
	, m_parentNode(parentNode)
	, m_newNode(NULL)
	, m_value(data->newValue())
	, m_metaData(data->newMetadata())
{
	TreeViewModel* model = m_parentNode->getChildren();
	kdb::Key newKey = model->createNewKey(m_parentNode->getPath() + "/" + data->newName(), m_value, m_metaData);

	QStringList newNameSplit = model->getSplittedKeyname(newKey);
	QStringList parentNameSplit = model->getSplittedKeyname(parentNode->getKey());

	//check if the new key is directly below the parent
	QSet<QString> diff = newNameSplit.toSet().subtract(parentNameSplit.toSet());

	if(diff.count() > 1 || isBelow)
		setText("newBranch");
	else
		setText("newKey");

	m_name = cutListAtIndex(newNameSplit, parentNameSplit.count()).first();

	model->sink(m_parentNode, newNameSplit, newKey.dup());

	m_newNode = m_parentNode->getChildByName(m_name);
	model->removeRow(m_parentNode->getChildIndexByName(m_name));
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

QStringList NewKeyCommand::cutListAtIndex(QStringList &list, int index)
{
	for(int i = 0; i < index; i++)
		list.removeFirst();

	return list;
}
