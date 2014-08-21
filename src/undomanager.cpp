#include <QUndoStack>
#include "undomanager.hpp"
#include "editcommand.hpp"
#include "deletecommand.hpp"

UndoManager::UndoManager(QObject *parent) :
    QObject(parent)
    , m_undoStack(new QUndoStack(this))
{
    connect(m_undoStack, SIGNAL(canRedoChanged(bool)), this, SIGNAL(canRedoChanged()));
    connect(m_undoStack, SIGNAL(canUndoChanged(bool)), this, SIGNAL(canUndoChanged()));

    m_undoStack->setUndoLimit(50);
}

UndoManager::UndoManager(const UndoManager &other)
{

}

UndoManager::~UndoManager()
{

}

bool UndoManager::canUndo() const
{
    return m_undoStack->canUndo();
}

bool UndoManager::canRedo() const
{
    return m_undoStack->canRedo();
}

void UndoManager::createEditCommand(TreeViewModel *model, int index, const QString &oldName, const QVariant &oldValue, const QVariant &oldMetaData,
                                    const QString &newName, const QVariant &newValue, const QVariant &newMetaData)
{
    //convert TreeViewModel to QVariantMap
    TreeViewModel *tmpModel = qvariant_cast<TreeViewModel*>(oldMetaData);
    QVariantMap oldMDMap;

    foreach(ConfigNode *node, tmpModel->model()){
        oldMDMap.insert(node->getName(), node->getValue());
    }

    m_undoStack->push(new EditCommand(model, index, oldName, oldValue, oldMDMap, newName, newValue, newMetaData.toMap()));
}

void UndoManager::createDeleteCommand(TreeViewModel *model, ConfigNode *node, int index)
{
    m_undoStack->push(new DeleteCommand(model, node, index));
}

void UndoManager::undo()
{
    m_undoStack->undo();
}

void UndoManager::redo()
{
    m_undoStack->redo();
}
