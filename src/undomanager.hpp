#ifndef UNDOMANAGER_HPP
#define UNDOMANAGER_HPP

#include <QObject>

class QUndoStack;

class UndoManager : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool canUndo READ canUndo() NOTIFY canUndoChanged())
    Q_PROPERTY(bool canRedo READ canRedo() NOTIFY canRedoChanged())

public:
    explicit UndoManager(QObject *parent = 0);
    UndoManager(UndoManager const & other);
    ~UndoManager();

    bool canUndo() const;
    bool canRedo() const;


Q_SIGNALS:

    void canUndoChanged();
    void canRedoChanged();

public Q_SLOTS:

    void undo();
    void redo();

private:

    QUndoStack *m_undoStack;
};

Q_DECLARE_METATYPE(UndoManager)

#endif // UNDOMANAGER_HPP
