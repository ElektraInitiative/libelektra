#ifndef EDITCOMMAND_HPP
#define EDITCOMMAND_HPP

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class EditCommand : public QUndoCommand
{
public:
    explicit EditCommand(TreeViewModel *model, int index, const QString &oldName, const QVariant &oldValue, const QVariant &oldMetaData,
                         const QString &newName, const QVariant &newValue, const QVariant &newMetaData, QUndoCommand *parent = 0);
    virtual void undo();
    virtual void redo();

private:

    TreeViewModel *m_model;
    int m_index;

    QString m_oldName;
    QString m_newName;
    QVariant m_oldMetaData;

    QVariant m_oldValue;
    QVariant m_newValue;
    QVariant m_newMetaData;
};

#endif // EDITCOMMAND_HPP
