#include <QUndoStack>
#include "undomanager.hpp"
#include "editkeycommand.hpp"
#include "deletekeycommand.hpp"
#include "newkeycommand.hpp"
#include "copykeycommand.hpp"
#include "cutkeycommand.hpp"
#include "importconfigurationcommand.hpp"

UndoManager::UndoManager(QObject *parent) :
    QObject(parent)
  , m_undoStack(new QUndoStack(this))
  , m_clipboardEmpty(true)
{
    connect(m_undoStack, SIGNAL(canRedoChanged(bool)), this, SIGNAL(canRedoChanged()));
    connect(m_undoStack, SIGNAL(canUndoChanged(bool)), this, SIGNAL(canUndoChanged()));
    connect(m_undoStack, SIGNAL(redoTextChanged(QString)), this, SIGNAL(redoTextChanged()));
    connect(m_undoStack, SIGNAL(undoTextChanged(QString)), this, SIGNAL(undoTextChanged()));

    m_clipboard = QApplication::clipboard();
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

void UndoManager::createEditKeyCommand(TreeViewModel *model, int index, const QString &oldName, const QVariant &oldValue, const QVariant &oldMetaData,
                                    const QString &newName, const QVariant &newValue, const QVariant &newMetaData)
{
    //convert TreeViewModel to QVariantMap
    TreeViewModel *tmpModel = qvariant_cast<TreeViewModel*>(oldMetaData);
    QVariantMap oldMDMap;

    foreach(ConfigNode *node, tmpModel->model()){
        oldMDMap.insert(node->getName(), node->getValue());
    }

    m_undoStack->push(new EditKeyCommand(model, index, oldName, oldValue, oldMDMap, newName, newValue, newMetaData.toMap()));
}

void UndoManager::createDeleteKeyCommand(const QString &type, TreeViewModel *model, ConfigNode *node, int index)
{
    m_undoStack->push(new DeleteKeyCommand(type, model, node, index));
}

void UndoManager::createNewKeyCommand(TreeViewModel *model, const QString &name, const QString &value, const QVariantMap &metaData)
{
    m_undoStack->push(new NewKeyCommand(model, name, value, metaData));
}

void UndoManager::createCopyKeyCommand(ConfigNode *target)
{
    m_undoStack->push(new CopyKeyCommand(m_clipboardType, qvariant_cast<ConfigNode*>(m_clipboard->property("node")), target));
}

void UndoManager::createCutKeyCommand(ConfigNode *target)
{
    m_undoStack->push(new CutKeyCommand(m_clipboardType, qvariant_cast<TreeViewModel*>(m_clipboard->property("model")), qvariant_cast<ConfigNode*>(m_clipboard->property("node")), target, m_clipboard->property("index").toInt()));
}

void UndoManager::createImportConfigurationCommand(TreeViewModel *model, const QString &name, const QString &format, const QString &file, const QString &mergeStrategy)
{
    m_undoStack->push(new ImportConfigurationCommand(model, name, format, file, mergeStrategy));
}

void UndoManager::setClean()
{
    m_undoStack->setClean();
}

bool UndoManager::isClean()
{
    return m_undoStack->isClean();
}

bool UndoManager::canPaste()
{
    return !m_clipboardEmpty;
}

TreeViewModel *UndoManager::getClipBoardModel() const
{
    if(!m_clipboardEmpty){
        return m_clipboardModel;
    }

    return NULL;
}

void UndoManager::undo()
{
    m_undoStack->undo();
}

void UndoManager::redo()
{
    m_undoStack->redo();
}

QString UndoManager::undoText() const
{
    return m_undoStack->undoText();
}

QString UndoManager::clipboardType() const
{
    return m_clipboardType;
}

void UndoManager::putToClipboard(const QString &type, TreeViewModel *model, ConfigNode *node, int index)
{
    m_clipboardType = type;

    m_clipboard->clear();

    m_clipboard->setProperty("model", QVariant::fromValue(model));
    m_clipboard->setProperty("node", QVariant::fromValue(new ConfigNode(*node)));
    m_clipboard->setProperty("index", QVariant::fromValue(index));

    m_clipboardEmpty = false;
    m_clipboardModel = model;

    emit clipboardTypeChanged();
    emit canPasteChanged();
}

QString UndoManager::redoText() const
{
    return m_undoStack->redoText();
}
