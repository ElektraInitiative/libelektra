#include "cutkeycommand.hpp"

CutKeyCommand::CutKeyCommand(QString type, TreeViewModel *model, ConfigNode *source, ConfigNode *target, int index, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_source(source)
    , m_target(target)
    , m_sourceIndex(index)
    , m_targetIndex(-1)
{
    setText(type);
}

void CutKeyCommand::undo()
{
    m_model->insertRow(m_sourceIndex, m_source);
    m_target->getChildren()->removeRow(m_targetIndex);

    if(m_target->childrenHaveNoChildren()){
        m_target->setIsExpanded(false);
    }

    if(m_model == m_target->getChildren()){
        m_model->reloadModel();
    }
}

void CutKeyCommand::redo()
{
    QString newPath = m_target->getPath() + "/" + m_source->getName();
    m_source->setPath(newPath);
    m_target->appendChild(new ConfigNode(*m_source));
    m_model->removeRow(m_sourceIndex);

    if(m_model == m_target->getChildren()){
        m_targetIndex = m_target->getChildCount();
        m_model->reloadModel();
    }
    else
        m_targetIndex = m_target->getChildCount() - 1;
}
