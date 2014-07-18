#ifndef TREEVIEWMODEL_H
#define TREEVIEWMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QDebug>
#include <QtQml>
#include <kdb.hpp>
#include <keyio.hpp>

#include "confignode.hpp"

class ConfigNode;

class TreeViewModel : public QAbstractListModel
{

	Q_OBJECT

public:

	// TODO: document roles
	enum TreeViewModelRoles
	{
	 NameRole = Qt::UserRole + 1,
	 PathRole,
	 ValueRole,
	 ChildCountRole,
	 ChildrenRole,
	 ChildrenHaveNoChildrenRole,
	 MetaValueRole,
	 RowCountRole,
	 NodeRole
	};

	explicit TreeViewModel(QObject* parent =  0);
	// TODO: is this constructor needed?
	TreeViewModel(QList<ConfigNode*> const & nodes);
	// Needed for Qt
	TreeViewModel(TreeViewModel const & other);
	~TreeViewModel();

	/// @return the underlying model
	QList<ConfigNode*>& model()
	{
		return m_model;
	}

	//mandatory methods inherited from QAbstractItemModel
	int                     rowCount(const QModelIndex& parent = QModelIndex()) const;
	Q_INVOKABLE int         qmlRowCount() const;
	QVariant                data(const QModelIndex& index, int role = Qt::DisplayRole) const;
	bool                    setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole);
	Q_INVOKABLE void        setDataValue(int index, const QVariant& value, const QString& role);
	bool                    insertRows(int row, int count, const QModelIndex& parent = QModelIndex());
	Q_INVOKABLE bool        removeRow(int row, const QModelIndex& parent = QModelIndex());
	Qt::ItemFlags           flags(const QModelIndex& index) const;

	/// recursively populate the model
	void populateModel(kdb::KeySet const & config);

	// TODO: add visitor in order to:
	// print tree for debugging purposes
	// get current KeySet (by appending all Keys in the ConfigNodes
	//    with this KeySet we can implement undo and save to storage

	// TODO: what are the methods for?
	Q_INVOKABLE QVariantMap get(int idx) const;
	Q_INVOKABLE QVariant    find(const QString& term);

private:
	void sink(ConfigNode* node, QStringList keys, QString path);
	void find(ConfigNode* node, const QString term);

	QList<ConfigNode*> m_model;
	// TODO: why are the searchResults in the same model??
	QList<ConfigNode*> m_searchResults;

protected:
	QHash<int, QByteArray> roleNames() const;

signals:
	void modelChanged();

public slots:

};

Q_DECLARE_METATYPE(TreeViewModel)

#endif // TREEVIEWMODEL_H
