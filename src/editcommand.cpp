#include "editcommand.hpp"

EditCommand::EditCommand(TreeViewModel *model, int index, const QString &oldName, const QVariant &oldValue, const QVariantMap &oldMetaData,
                         const QString &newName, const QVariant &newValue, const QVariantMap &newMetaData, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_index(index)
    , m_oldName(oldName)
    , m_oldValue(oldValue)
    , m_oldMetaData(oldMetaData)
    , m_newName(newName)
    , m_newValue(newValue)
    , m_newMetaData(newMetaData)
{

    qDebug() << "oldname " << m_oldName;
    qDebug() << "oldvalue " << m_oldValue.toString();
    qDebug() << "newname " << m_newName;
    qDebug() << "newvalue " << m_newValue.toString();

}

void EditCommand::undo()
{
//    qDebug() << "oldModeldata " << m_oldMetaData;

    m_model->setData(m_index, m_oldName, "Name");
    m_model->setData(m_index, m_oldValue, "Value");
    m_model->model().at(m_index)->setMeta(m_oldMetaData);
}

void EditCommand::redo()
{
//    qDebug() << "newModeldata " << m_newMetaData;
    m_model->setData(m_index, m_newName, "Name");
    m_model->setData(m_index, m_newValue, "Value");
    m_model->model().at(m_index)->setMeta(m_newMetaData);
}
