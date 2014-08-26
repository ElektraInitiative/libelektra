#include "newkeycommand.hpp"

NewKeyCommand::NewKeyCommand(TreeViewModel *model, const QString &name, const QString &value, const QVariantMap &metaData, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_name(name)
    , m_value(value)
    , m_metaData(metaData)
{
    setText("new");
}

void NewKeyCommand::undo()
{

}

void NewKeyCommand::redo()
{
    m_model->createNewNode(m_name, m_value, m_metaData);
}
