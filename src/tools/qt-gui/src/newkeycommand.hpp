#ifndef NEWKEYCOMMAND_HPP
#define NEWKEYCOMMAND_HPP

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class NewKeyCommand : public QUndoCommand
{

public:
    explicit NewKeyCommand(TreeViewModel *model, const QString &path, const QString &value, const QVariantMap &metaData, QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();

private:

    TreeViewModel *m_model;
    QString m_path;
    QString m_value;
    QVariantMap m_metaData;

};

#endif // NEWKEYCOMMAND_HPP
