#include "cutkeycommand.hpp"

CutKeyCommand::CutKeyCommand(QString type, TreeViewModel *sourceParentModel, ConfigNode *source, ConfigNode *target, int index, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_sourceParentModel(sourceParentModel)
    , m_source(source)
    , m_target(target)
    , m_sourceIndex(index)
    , m_targetIndex(-1)
{
    setText(type);
}

void CutKeyCommand::undo()
{
    m_sourceParentModel->insertRow(m_sourceIndex, m_source);
    m_target->getChildren()->removeRow(m_targetIndex);

    if(m_sourceParentModel == m_target->getChildren()){
        m_sourceParentModel->refresh();
    }
}

void CutKeyCommand::redo()
{
    QString newPath = m_target->getPath() + "/" + m_source->getName();
    m_source->setPath(newPath);
    m_target->appendChild(new ConfigNode(*m_source));
    m_sourceParentModel->removeRow(m_sourceIndex);

    if(m_sourceParentModel == m_target->getChildren()){
        m_targetIndex = m_target->getChildCount();
        m_sourceParentModel->refresh();
    }
    else
        m_targetIndex = m_target->getChildCount() - 1;
}
