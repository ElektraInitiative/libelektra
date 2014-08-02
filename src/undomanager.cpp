#include <QUndoStack>
#include "undomanager.hpp"

UndoManager::UndoManager(QObject *parent) :
    QObject(parent)
    , m_undoStack(new QUndoStack(this))
{
    connect(m_undoStack, SIGNAL(canRedoChanged(bool)), this, SIGNAL(canRedoChanged()));
    connect(m_undoStack, SIGNAL(canUndoChanged(bool)), this, SIGNAL(canUndoChanged()));
}

UndoManager::UndoManager(const UndoManager &other)
{

}

UndoManager::~UndoManager()
{

}

bool UndoManager::canUndo() const
{
    return false;
}

bool UndoManager::canRedo() const
{
    return false;
}

void UndoManager::redo()
{
    m_undoStack->redo();
}

void UndoManager::undo()
{
    m_undoStack->undo();
}
