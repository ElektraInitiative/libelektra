/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef UNDOMANAGER_HPP
#define UNDOMANAGER_HPP

#include "./datacontainer.hpp"
#include "./treeviewmodel.hpp"
#include <QApplication>
#include <QClipboard>
#include <QObject>

class QUndoStack;

/**
 * @brief The UndoManager class
 *
 * To add your own command, implement a class that inherits from QUndoCommand. This class will have an undo and redo method respectively.
 * ATTENTION: you have to put the code of THE FIRST ACTION into the redo method. Redo will be called automatically when the command is
 * pushed onto the QUndoStack. If you put the code of the initial action somewhere in your code and then create the new command, the action
 * will be performed twice. In the undo method you have to put the code that reverts the initial action. To execute the command, push your
 * new command on the stack.
 */
class UndoManager : public QObject
{
	Q_OBJECT

	Q_PROPERTY (bool canUndo READ canUndo () NOTIFY canUndoChanged ())
	Q_PROPERTY (bool canRedo READ canRedo () NOTIFY canRedoChanged ())
	Q_PROPERTY (bool canPaste READ canPaste () NOTIFY canPasteChanged ())
	Q_PROPERTY (QString redoText READ redoText () NOTIFY redoTextChanged ())
	Q_PROPERTY (QString undoText READ undoText () NOTIFY undoTextChanged ())
	Q_PROPERTY (QString clipboardType READ clipboardType () NOTIFY clipboardTypeChanged ())

public:
	/**
	 * @brief The default constructor.
	 * @param parentManager
	 */
	explicit UndoManager (QObject * parentManager = nullptr);

	/**
	 * @brief The mandatory copy constructor.
	 * @param other
	 */
	UndoManager (UndoManager const & other) : QObject ()
	{
		Q_UNUSED (other)
	}

	/**
	 * @brief Returns if a command can be undone.
	 *
	 * @return True if a command can be undone.
	 */
	bool canUndo () const;

	/**
	 * @brief Returns if a command can be redone.
	 *
	 * @return True if a command can be redone.
	 */
	bool canRedo () const;

	/**
	 * @brief Returns a textual description of the command on top of the UndoStack.
	 *
	 * @return A textual description of the command on top of the UndoStack.
	 */
	QString redoText () const;

	/**
	 * @brief Returns a textual description of the command on top of the UndoStack.
	 *
	 * @return A textual description of the command on top of the UndoStack.
	 */
	QString undoText () const;

	/**
	 * @brief Returns a textual description of the type of content currently in the clipboard.
	 *
	 * @return A textual description of the type of content currently in the clipboard.
	 */
	QString clipboardType () const;

	/**
	 * @brief Put some content into the clipboard.
	 *
	 * @param type A textual description of the type of content.
	 * @param model The TreeViewModel to put in the clipboard.
	 * @param idx The index of the ConfigNode.
	 */
	Q_INVOKABLE void putToClipboard (const QString & type, TreeViewModel * model, int idx);

	/**
	 * @brief Create a new EditKeyCommand.
	 *
	 * @param model The TreeViewModel of the edited ConfigNode.
	 * @param idx The index of the edited ConfigNode.
	 * @param data This container holds the data needed to undo and redo the edit.
	 */
	Q_INVOKABLE void createEditKeyCommand (TreeViewModel * model, int idx, DataContainer * data);

	/**
	 * @brief Create a new DeleteKeyCommand.
	 *
	 * @param type The type of the command ("deleteKey" or "deleteBranch").
	 * @param model The TreeViewModel of the deleted ConfigNode.
	 * @param idx The index of the deleted ConfigNode.
	 */
	Q_INVOKABLE void createDeleteKeyCommand (const QString & type, TreeViewModel * model, int idx);

	/**
	 * @brief Create a new NewKeyCommand.
	 *
	 * @param model The TreeViewModel of the new ConfigNode.
	 * @param idx The index of the new ConfigNode.
	 * @param data This container holds the data needed to undo and redo the edit.
	 * @param isBelow Determines if the new key is a treebranch that requires the treeview to update.
	 */
	Q_INVOKABLE void createNewKeyCommand (TreeViewModel * model, int idx, DataContainer * data, bool isBelow);

	/**
	 * @brief Copy a ConfigNode into a different TreeViewModel.
	 * @param model The target TreeViewModel of the copied ConfigNode.
	 * @param idx The index of the copied ConfigNode.
	 */
	Q_INVOKABLE void createCopyKeyCommand (TreeViewModel * model, int idx);

	/**
	 * @brief Copy a ConfigNode into a different TreeViewModel and delete the source ConfigNode.
	 * @param model The target TreeViewModel of the cut ConfigNode.
	 * @param idx The index of the cut ConfigNode.
	 */
	Q_INVOKABLE void createCutKeyCommand (TreeViewModel * model, int idx);

	/**
	 * @brief Import a configuration from file.
	 * @param model The TreeViewModel to paste the configuration into.
	 * @param idx The index of the ConfigNode that is the root of the imported configuration.
	 * @param data This container holds the data needed to undo and redo the import.
	 */
	Q_INVOKABLE void createImportConfigurationCommand (TreeViewModel * model, int idx, DataContainer * data);

	/**
	 * @brief This function is called when the configuration is saved; if the user closes the application
	 * in a clean state, she will not be asked to save the configuration again.
	 *
	 */
	Q_INVOKABLE void setClean ();

	/**
	 * @brief Returns if the UndoStack is in a clean state.
	 *
	 * @return True if the UndoStack is in a clean state.
	 */
	Q_INVOKABLE bool isClean () const;

	/**
	 * @brief Returns if the clipboard is filled.
	 *
	 * @return True if the clipboard is filled.
	 */
	Q_INVOKABLE bool canPaste () const;

	/**
	 * @brief Returns the current index of the UndoStack.
	 *
	 * @return The current index of the UndoStack.
	 */

	Q_INVOKABLE int index () const;

	/**
	 * @brief Returns the clean index of the UndoStack.
	 *
	 * @return The clean index of the UndoStack.
	 */
	Q_INVOKABLE int cleanIndex () const;

	/**
	 * @brief Returns the size of the UndoStack.
	 *
	 * @return The size of the UndoStack..
	 */
	Q_INVOKABLE int count () const;

	/**
	 * @brief setIndex
	 * @param idx
	 */
	Q_INVOKABLE void setIndex (int idx);

Q_SIGNALS:
	void canUndoChanged ();
	void canRedoChanged ();
	void redoTextChanged ();
	void undoTextChanged ();
	void clipboardTypeChanged ();
	void canPasteChanged ();

public Q_SLOTS:
	void undo ();
	void redo ();

private:
	QUndoStack * m_undoStack;
	QClipboard * m_clipboard;
	QString m_clipboardType;
	bool m_clipboardEmpty;
	TreeViewModel * m_clipboardModel;
};

Q_DECLARE_METATYPE (UndoManager)

#endif // UNDOMANAGER_HPP
