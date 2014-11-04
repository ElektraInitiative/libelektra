#include "newkeycommand.hpp"

NewKeyCommand::NewKeyCommand(ConfigNode *parentNode, const QString &path, const QString &value, const QVariantMap &metaData, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_parentNode(parentNode)
    , m_newNode(NULL)
    , m_path(path)
    , m_name(m_path.split("/").at(0))
    , m_value(value)
    , m_metaData(metaData)
{
    setText("newKey");

    m_parentNode->getChildren()->sink(m_parentNode, m_path.split("/"), m_path, m_parentNode->getChildren()->createNewKey(m_parentNode->getPath() + "/" + m_path, m_value, m_metaData));
    m_newNode = m_parentNode->getChildByName(m_name);
    m_newNode->setPath(m_parentNode->getPath() + "/" + m_name);
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
