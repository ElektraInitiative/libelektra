#include "newkeycommand.hpp"

NewKeyCommand::NewKeyCommand(TreeViewModel *model, const QString &path, const QString &value, const QVariantMap &metaData, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_path(path)
    , m_value(value)
    , m_metaData(metaData)
{
    setText("new");
}

void NewKeyCommand::undo()
{
    m_model->deletePath(m_path);
}

void NewKeyCommand::redo()
{
    m_model->createNewNode(m_path, m_value, m_metaData);
}
