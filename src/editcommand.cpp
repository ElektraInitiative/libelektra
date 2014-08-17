#include "editcommand.hpp"

EditCommand::EditCommand(TreeViewModel *model, int index, const QString &oldName, const QVariant &oldValue, const QVariant &oldMetaData,
                         const QString &newName, const QVariant &newValue, const QVariant &newMetaData, QUndoCommand *parent)
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

    m_model->setDataValue(m_index, m_oldName, "Name");
    m_model->setDataValue(m_index, m_oldValue, "Value");
//    m_model->setDataValue(m_index, m_oldMetaData, "MetaValue");
}

void EditCommand::redo()
{
//    qDebug() << "newModeldata " << m_newMetaData;
    m_model->setDataValue(m_index, m_newName, "Name");
    m_model->setDataValue(m_index, m_newValue, "Value");
    //m_model->setDataValue(m_index, m_newMetaData, "MetaValue");
}
