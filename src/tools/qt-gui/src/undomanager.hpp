
#ifndef UNDOMANAGER_HPP
#define UNDOMANAGER_HPP

#include <QObject>
#include <QApplication>
#include <QClipboard>
#include "treeviewmodel.hpp"

class QUndoStack;

/**
 * @brief The UndoManager class
 *
 * To add your own command, implement a class that inherits from QUndoCommand. This class will have an undo and redo method respectively.
 * ATTENTION: you put the code of THE FIRST ACTION into the redo method. Redo will be called automatically when the command is put onto the QUndoStack.
 * If you put the code of the initial action somewhere in your code and then put the command on the stack, the action will be performed twice. In the undo
 * method you put the code that reverts the initial action. To execute the command, put your new command on the stack.
 */
class UndoManager : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool canUndo READ canUndo() NOTIFY canUndoChanged())
    Q_PROPERTY(bool canRedo READ canRedo() NOTIFY canRedoChanged())

    Q_PROPERTY(QString redoText READ redoText() NOTIFY redoTextChanged())
    Q_PROPERTY(QString undoText READ undoText() NOTIFY undoTextChanged())
    Q_PROPERTY(QString clipboardType READ clipboardType() NOTIFY clipboardTypeChanged())

public:

    /**
     * @brief
     *
     * @param parent
     */
    explicit    UndoManager(QObject *parent = 0);

    /**
     * @brief
     *
     * @param other
     */
    UndoManager(UndoManager const & other);

    /**
     * @brief The destructor of this class.
     *
     */
    ~UndoManager();

    /**
     * @brief Returns if a command can be undone.
     *
     * @return True if a command can be undone.
     */
    bool canUndo() const;

    /**
     * @brief Returns if a command can be redone.
     *
     * @return True if a command can be redone.
     */
    bool canRedo() const;

    /**
     * @brief Returns a textual description of the command on top of the UndoStack.
     *
     * @return A textual description of the command on top of the UndoStack.
     */
    QString redoText() const;

    /**
     * @brief Returns a textual description of the command on top of the UndoStack.
     *
     * @return A textual description of the command on top of the UndoStack.
     */
    QString undoText() const;

    /**
     * @brief Returns a textual description of the type of content currently in the clipboard.
     *
     * @return A textual description of the type of content currently in the clipboard.
     */
    QString clipboardType() const;

    /**
     * @brief Put some content into the clipboard.
     *
     * @param type A textual description of the type of content.
     * @param model The TreeViewModel to put in the clipboard.
     * @param node The ConfigNode to put in the clipboard.
     * @param index The index of the ConfigNode.
     */
    Q_INVOKABLE void putToClipboard(const QString &type, TreeViewModel *model, ConfigNode *node, int index);

    /**
     * @brief Create a new EditKeyCommand.
     *
     * @param model The TreeViewModel of the edited ConfigNode.
     * @param index The index of the edited ConfigNode.
     * @param oldName The old name of the edited ConfigNode.
     * @param oldValue The old value of the edited ConfigNode.
     * @param oldMetaData The old meta data of the edited ConfigNode.
     * @param newName The new name of the edited ConfigNode.
     * @param newValue The new value of the edited ConfigNode.
     * @param newMetaData The new meta data of the edited ConfigNode.
     */
    Q_INVOKABLE void createEditKeyCommand(TreeViewModel *model, int index, const QString &oldName, const QVariant &oldValue, const QVariant &oldMetaData,
                                                 const QString &newName, const QVariant &newValue, const QVariant &newMetaData);

    /**
     * @brief Create a new DeleteKeyCommand.
     *
     * @param type The type of the command ("deleteKey" or "deleteBranch").
     * @param model The TreeViewModel of the deleted ConfigNode.
     * @param node The deleted ConfigNode.
     * @param index The index of the deleted ConfigNode.
     */
    Q_INVOKABLE void createDeleteKeyCommand(const QString &type, TreeViewModel *model, ConfigNode *node, int index);

    /**
     * @brief Create a new NewKeyCommand.
     *
     * @param model The TreeViewModel of the new ConfigNode.
     * @param name The name of the new ConfigNode.
     * @param value The value of the new ConfigNode.
     * @param metaData The meta data of the new ConfigNode.
     */
    Q_INVOKABLE void createNewKeyCommand(TreeViewModel *model, const QString &name, const QString &value, const QVariantMap &metaData);

    /**
     * @brief Create a new CopyKeyCommand.
     *
     * @param target The ConfigNode the copied ConfigNode will be the new child of.
     */
    Q_INVOKABLE void createCopyKeyCommand(ConfigNode *target);

    /**
     * @brief Create a new CutKeyCommand.
     *
     * @param target The ConfigNode the cut ConfigNode will be the new child of.
     */
    Q_INVOKABLE void createCutKeyCommand(ConfigNode *target);

    /**
     * @brief This function is called when the configuration is saved; if the user closes the application
     * in a clean state, she will not be asked to save the configuration again.
     *
     */
    Q_INVOKABLE void setClean();

    /**
     * @brief Returns if the UndoStack is in a clean state.
     *
     * @return True if the UndoStack is in a clean state.
     */
    Q_INVOKABLE bool isClean();

Q_SIGNALS:

    void canUndoChanged();

    void canRedoChanged();

    void redoTextChanged();

    void undoTextChanged();

    void clipboardTypeChanged();

public Q_SLOTS:

    void undo();

    void redo();

private:

    QUndoStack *m_undoStack;
    QClipboard *m_clipboard;
    QString     m_clipboardType;
};

Q_DECLARE_METATYPE(UndoManager)

#endif // UNDOMANAGER_HPP
