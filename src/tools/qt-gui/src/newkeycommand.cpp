/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./newkeycommand.hpp"
#include <kdb.hpp>

NewKeyCommand::NewKeyCommand (TreeViewModel * model, int index, DataContainer * data, bool isBelow, QUndoCommand * parent)
: QUndoCommand (parent), m_parentNode (model->model ().at (index)), m_newNode (nullptr), m_value (data->newValue ()),
  m_metaData (data->newMetadata ())
{
	TreeViewModel * parentModel = m_parentNode->getChildren ();
	kdb::Key newKey = parentModel->createNewKey (m_parentNode->getPath () + "/" + data->newName (), m_value, m_metaData);

	QStringList newNameSplit = parentModel->getSplittedKeyname (newKey);
	kdb::Key parentKey = m_parentNode->getKey ();

	if (!parentKey)
	{
		std::string parentPath = m_parentNode->getPath ().toStdString ();
		if (parentPath.find ('/') == std::string::npos)
		{
			parentPath += "/";
		}

		parentKey = kdb::Key (parentPath, KEY_END);
	}

	QStringList parentNameSplit = parentModel->getSplittedKeyname (parentKey);

	// check if the new key is directly below the parent
	QSet<QString> diff = newNameSplit.toSet ().subtract (parentNameSplit.toSet ());

	if (diff.count () > 1 || isBelow)
		setText ("newBranch");
	else
		setText ("newKey");

	m_name = cutListAtIndex (newNameSplit, parentNameSplit.count ()).first ();

	parentModel->sink (m_parentNode, newNameSplit, newKey.dup ());

	m_newNode = m_parentNode->getChildByName (m_name);
	parentModel->removeRow (m_parentNode->getChildIndexByName (m_name));
}

void NewKeyCommand::undo ()
{
	// remove new node
	m_parentNode->getChildren ()->removeRow (m_parentNode->getChildIndexByName (m_name));
}

void NewKeyCommand::redo ()
{
	// insert new node
	m_parentNode->getChildren ()->append (m_newNode);
}

QStringList NewKeyCommand::cutListAtIndex (QStringList & list, int index)
{
	for (int i = 0; i < index; i++)
		list.removeFirst ();

	return list;
}
