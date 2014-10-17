#include "cutkeycommand.hpp"

CutKeyCommand::CutKeyCommand(TreeViewModel *model, ConfigNode *source, ConfigNode *target, int index, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_source(*source)
    , m_target(target)
    , m_index(index)
{
    setText("cut");
}

void CutKeyCommand::undo()
{
    m_model->insertRow(m_index, new ConfigNode(m_source));
    m_target->getChildren()->removeRow(m_target->getIndexByName(m_source.getName()));
}

void CutKeyCommand::redo()
{
    QString newPath = m_target->getPath() + "/" + m_source.getName();
    m_source.setPath(newPath);
    m_source.setKeyName(newPath);

    m_target->appendChild(new ConfigNode(m_source));
    m_model->removeRow(m_index);
}
