#ifndef UNDOMANAGER_HPP
#define UNDOMANAGER_HPP

#include <QObject>
#include <QApplication>
#include <QClipboard>
#include "treeviewmodel.hpp"

class QUndoStack;

class UndoManager : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool canUndo READ canUndo() NOTIFY canUndoChanged())
    Q_PROPERTY(bool canRedo READ canRedo() NOTIFY canRedoChanged())

    Q_PROPERTY(QString redoText READ redoText() NOTIFY redoTextChanged())
    Q_PROPERTY(QString undoText READ undoText() NOTIFY undoTextChanged())
    Q_PROPERTY(QString clipboardType READ clipboardType() NOTIFY clipboardTypeChanged())

public:

                explicit    UndoManager(QObject *parent = 0);
                            UndoManager(UndoManager const & other);
                            ~UndoManager();

                bool        canUndo() const;
                bool        canRedo() const;

                QString     redoText() const;
                QString     undoText() const;
                QString     clipboardType() const;

    Q_INVOKABLE void        putToClipboard(const QString &type, TreeViewModel *model, ConfigNode *node, int index);

    Q_INVOKABLE void        createEditKeyCommand(TreeViewModel *model, int index, const QString &oldName, const QVariant &oldValue, const QVariant &oldMetaData,
                                          const QString &newName, const QVariant &newValue, const QVariant &newMetaData);

    Q_INVOKABLE void        createDeleteKeyCommand(const QString &type, TreeViewModel *model, ConfigNode *node, int index);
    Q_INVOKABLE void        createNewKeyCommand(TreeViewModel *model, const QString &name, const QString &value, const QVariantMap &metaData);
    Q_INVOKABLE void        createCopyKeyCommand(ConfigNode *target);
    Q_INVOKABLE void        createCutKeyCommand(ConfigNode *target);
    Q_INVOKABLE void        setClean();
    Q_INVOKABLE bool        isClean();

Q_SIGNALS:

                void        canUndoChanged();
                void        canRedoChanged();
                void        redoTextChanged();
                void        undoTextChanged();
                void        clipboardTypeChanged();

public Q_SLOTS:

                void         undo();
                void         redo();

private:

    QUndoStack *m_undoStack;
    QClipboard *m_clipboard;
    QString     m_clipboardType;
};

Q_DECLARE_METATYPE(UndoManager)

#endif // UNDOMANAGER_HPP
