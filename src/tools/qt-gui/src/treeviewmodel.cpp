#include "treeviewmodel.hpp"
#include <threewaymerge.hpp>
#include <mergeconfiguration.hpp>
#include <automergeconfiguration.hpp>
#include <onesidemergeconfiguration.hpp>
#include <overwritemergeconfiguration.hpp>
#include <importmergeconfiguration.hpp>
#include <external.hpp>
#include <toolexcept.hpp>
#include <backends.hpp>
#include <modules.hpp>
#include <plugin.hpp>
#include <plugins.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::merging;

TreeViewModel::TreeViewModel(QObject* parentModel)
{
	Q_UNUSED(parentModel);
}

TreeViewModel::TreeViewModel(const TreeViewModel& other)
	: QAbstractListModel()
{
	m_model = other.m_model; // copy from other list
}

int TreeViewModel::rowCount(const QModelIndex& parentIndex) const
{
	Q_UNUSED(parentIndex);
	return m_model.count();
}

QVariant TreeViewModel::data(const QModelIndex& idx, int role) const
{
	if (!idx.isValid())
	{
		emit showMessage(tr("Error"), tr("Index not valid."), QString("TreeViewModel::data: Index = %1,\nModel size = %2").arg(idx.row()).arg(m_model.count()));
		return QVariant();
	}

	if (idx.row() > (m_model.size() - 1))
	{
		emit showMessage(tr("Error"), QString(tr("Index too high. ")), QString("TreeViewModel::data: Index: %1").arg(idx.row()));
		return QVariant();
	}

	ConfigNodePtr node = m_model.at(idx.row());

	switch (role)
	{

	case Qt::DisplayRole:

		// TODO: document fallthrough if it was desired
	case NameRole:
		return QVariant::fromValue(node->getName());

	case PathRole:
		return QVariant::fromValue(node->getPath());

	case ValueRole:
		return QVariant::fromValue(node->getValue());

	case ChildCountRole:
		return QVariant::fromValue(node->getChildCount());

	case ChildrenRole:
		return QVariant::fromValue(node->getChildren());

	case ChildrenHaveNoChildrenRole:
		return QVariant::fromValue(node->childrenHaveNoChildren());

	case MetaValueRole:
		return QVariant::fromValue(node->getMetaKeys());

	case NodeRole:
		return QVariant::fromValue(node.data());

	case ParentModelRole:
		return QVariant::fromValue(node->getParentModel());

	case IndexRole:
		return QVariant::fromValue(idx.row());

	case IsNullRole:
	{
		if (node->getKey())
			return QVariant::fromValue(false);
		else
			return QVariant::fromValue(true);
	}

	case IsExpandedRole:
		return QVariant::fromValue(node->isExpanded());

	default:
		emit showMessage(tr("Error"), tr("Unknown role: %1").arg(role), "TreeViewModel::data");
		return QVariant();
	}
}

bool TreeViewModel::setData(const QModelIndex& idx, const QVariant& modelData, int role)
{
	if (!idx.isValid() || idx.row() > (m_model.size() - 1))
	{
		emit showMessage(tr("Error"), tr("Index not valid."), QString("TreeViewModel::setData: Index = %1,\nModel size = %2").arg(idx.row()).arg(m_model.count()));
		return false;
	}

	ConfigNodePtr node = m_model.at(idx.row());

	switch (role)
	{

	case NameRole:
		node->setName(modelData.toString());
		break;

	case ValueRole:
		node->setValue(modelData);
		break;

	case MetaValueRole:
	{
		QVariantList valueList = modelData.toList();
		node->setMeta(valueList.at(0).toString(), valueList.at(1));
		break;
	}

	case IsExpandedRole:
		node->setIsExpanded(modelData.toBool());
	}

	emit dataChanged(idx, idx);

	return true;
}

int TreeViewModel::getIndexByName(const QString& name) const
{
	for (int i = 0; i < m_model.count(); i++)
	{
		if (m_model.at(i)->getName() == name)
			return i;
	}

	return -1;
}

void TreeViewModel::importConfiguration(const QString& name, const QString& format, QString& file, const QString& mergeStrategy)
{
	Key		root(name.toStdString(), KEY_END);
	KeySet	originalKeys = collectCurrentKeySet();
	KeySet	base = originalKeys.cut(root);
	printWarnings (cerr, root);

	KeySet importedKeys;

	string formatString = format.toStdString();
	string fileString = file.remove("file://").toStdString();

	Modules modules;
	PluginPtr plugin = modules.load (formatString);

	Key errorKey (root);
	errorKey.setString (fileString);

	plugin->get (importedKeys, errorKey);

	printWarnings (cerr, errorKey);
	printError (cerr, errorKey);

	ThreeWayMerge			merger;
	AutoMergeConfiguration*	configuration;

	if(mergeStrategy == "preserve")
		configuration = new AutoMergeConfiguration();
	else if(mergeStrategy == "ours")
		configuration = new OneSideMergeConfiguration(OURS);
	else if(mergeStrategy == "theirs")
		configuration = new OneSideMergeConfiguration(THEIRS);
	else if(mergeStrategy == "cut")
		configuration = new OverwriteMergeConfiguration(THEIRS);
	else if(mergeStrategy == "import")
		configuration = new ImportMergeConfiguration();

	configuration->configureMerger(merger);

	MergeResult result;

	try
	{
		result = merger.mergeKeySet(MergeTask (BaseMergeKeys (base, root), OurMergeKeys (base, root), TheirMergeKeys (importedKeys, root), root));
	}
	catch(...)//TODO
	{
		qDebug() << "Could not merge keysets";
	}

	if (!result.hasConflicts ())
	{
		createNewNodes(result.getMergedKeys());
	}
	else
	{
		emit showMessage(tr("Error"), tr("Importing the configuration from file caused conflicts."), "");
	}
}

void TreeViewModel::exportConfiguration(TreeViewModel* parentModel, int idx, QString format, QString file)
{
	KeySet	ks = collectCurrentKeySet();
	Key		root = parentModel->model().at(idx)->getKey();

	//Node is only a filler
	if(!root)
		root = Key(parentModel->model().at(idx)->getPath().toStdString(), KEY_END);

	KeySet part(ks.cut(root));

	string formatString = format.toStdString();
	string fileString = file.remove("file://").toStdString();

	Modules modules;
	PluginPtr plugin = modules.load(formatString);

	Key errorKey(root);
	errorKey.setString(fileString);

	plugin->set(part, errorKey);

	printWarnings(cerr, errorKey);
	printError(cerr, errorKey);
}

KeySet TreeViewModel::collectCurrentKeySet()
{
	KeySetVisitor ksVisit;
	accept(ksVisit);

	return ksVisit.getKeySet();
}

Qt::ItemFlags TreeViewModel::flags(const QModelIndex& idx) const
{
	if (!idx.isValid())
		return Qt::ItemIsEnabled;

	return QAbstractItemModel::flags(idx) | Qt::ItemIsEditable | Qt::ItemIsDragEnabled | Qt::ItemIsDropEnabled;
}

void TreeViewModel::accept(Visitor& visitor)
{
	visitor.visit(this);
}

QVariantMap TreeViewModel::get(const int& idx) const
{
	QVariantMap map;

	foreach (int k, roleNames().keys())
	{
		map[roleNames().value(k)] = data(index(idx, 0), k);
	}

	return map;
}

QVariant TreeViewModel::find(const QString& term)
{
	TreeViewModel* searchResults = new TreeViewModel;
	FindVisitor fVisit(searchResults, term);
	accept(fVisit);

	if (searchResults->rowCount() == 0)
	{
		searchResults->model().append(ConfigNodePtr(new ConfigNode("NotfoundNode", tr("There were no results matching your query."), 0, this)));
	}

	return QVariant::fromValue(searchResults);
}

bool TreeViewModel::removeRow(int row, const QModelIndex& parentIndex)
{
	Q_UNUSED(parentIndex);

	if (row < 0 || row > m_model.size() - 1)
	{
		emit showMessage(tr("Error"), tr("Index not valid."), QString("TreeViewModel::removeRow: Index = %1, Model size = %2").arg(m_model.size()).arg(row));
		return false;
	}

	beginRemoveRows(QModelIndex(), row, row);

	if (!m_model.isEmpty())
		m_model.removeAt(row);

	endRemoveRows();

	int childCount = 0;

	foreach (ConfigNodePtr node, m_model)
	{
		childCount += node->getChildCount();
	}

	if (childCount == 0)
		emit expandNode(false);

	emit updateIndicator();

	return true;
}

bool TreeViewModel::insertRow(int row, const QModelIndex& parentIndex)
{
	Q_UNUSED(parentIndex);
	ConfigNodePtr node(new ConfigNode);
	node->setName(QString::fromStdString(m_metaModelParent.getName()));
	node->setKey(m_metaModelParent);
	node->setParentModel(this);

	beginInsertRows(QModelIndex(), row, row);
	m_model.insert(row, node);
	endInsertRows();

	emit updateIndicator();

	return true;
}

void TreeViewModel::insertRow(int row, ConfigNodePtr node, bool addParent)
{
	beginInsertRows(QModelIndex(), row, row);
	if(addParent)
		node->setParentModel(this);
	m_model.insert(row, node);
	endInsertRows();

	emit updateIndicator();
}

void TreeViewModel::insertMetaRow(int row, Key key, const QString &name)
{
	m_metaModelParent = key;

	if (m_metaModelParent)
	{
		insertRow(row);
	}
	else
	{
		QString keyName;

		if (key)
			keyName = QString::fromStdString(key.getFullName());
		else
			keyName = name;

		emit showMessage(tr("Error"), tr("Inserting of Metakey failed, \"%1\" is not valid.").arg(keyName), "");
	}
}

void TreeViewModel::sink(ConfigNodePtr node, QStringList keys, const Key& key)
{
	if (keys.length() == 0)
		return;

	bool isLeaf = (keys.length() == 1);

	QString name =  keys.takeFirst();

	if (node->hasChild(name))
	{
		sink(node->getChildByName(name), keys, key);
	}
	else
	{
		ConfigNodePtr newNode;

		if (isLeaf)
			newNode = ConfigNodePtr(new ConfigNode(name, (node->getPath() + "/" + name), key, node->getChildren()));
		else
			newNode = ConfigNodePtr(new ConfigNode(name, (node->getPath() + "/" + name), NULL, node->getChildren()));

		node->appendChild(newNode);

		sink(newNode, keys, key);
	}
}

void TreeViewModel::populateModel(KeySet keySet)
{
	ConfigNodePtr spec(new ConfigNode("spec", "spec", 0, this));
	ConfigNodePtr proc(new ConfigNode("proc", "proc", 0, this));
	ConfigNodePtr dir(new ConfigNode("dir", "dir", 0, this));
	ConfigNodePtr user(new ConfigNode("user", "user", 0, this));
	ConfigNodePtr system(new ConfigNode("system", "system", 0, this));
	ConfigNodePtr cascading(new ConfigNode("cascading", "cascading", 0, this));

	m_model.clear();
	m_model << spec << proc << dir << user << system << cascading;

	createNewNodes(keySet);
}

void TreeViewModel::createNewNodes(KeySet keySet)
{
	keySet.rewind();

	while (keySet.next())
	{
		Key k = keySet.current().dup();
		QStringList keys = getSplittedKeyname(k);
		QString root = keys.takeFirst();

		for(int i = 0; i < m_model.count(); i++)
		{
			if(root == m_model.at(i)->getName())
				sink(m_model.at(i), keys, k);
		}
	}
}

Key TreeViewModel::createNewKey(const QString& path, const QString& value, const QVariantMap metaData)
{
	Key key;
	key.setName(path.toStdString());
	key.setString(value.toStdString());

	for (QVariantMap::const_iterator iter = metaData.begin(); iter != metaData.end(); iter++)
	{
		key.setMeta(iter.key().toStdString(), iter.value().toString().toStdString());
	}

	return key;
}

void TreeViewModel::append(ConfigNodePtr node)
{
	insertRow(rowCount(), node);
}

void TreeViewModel::synchronize()
{
	KDB kdb;
	KeySet keySet;

	try
	{
		kdb.get(keySet, "/");
	}
	catch (KDBException const& e)
	{
		emit showMessage(tr("Error"), tr("Could not read from configuration."), e.what());
	}

	keySet = collectCurrentKeySet();

	try
	{
		kdb.set(keySet, "user");
	}
	catch (KDBException const& e)
	{
		emit showMessage(tr("Error"), tr("Could not write to configuration."), e.what());
	}
}

void TreeViewModel::clearMetaModel()
{
	beginResetModel();
	m_model.clear();
	endResetModel();
}

void TreeViewModel::unMountBackend(QString backendName)
{
	KeySet keySet = collectCurrentKeySet();
	Backends::umount(backendName.toStdString(), keySet);
	populateModel(keySet);
	synchronize();
}

void TreeViewModel::refresh()
{
	layoutAboutToBeChanged();
	layoutChanged();
}

QString TreeViewModel::getCurrentArrayNo() const
{
	ConfigNodePtr max(NULL);

	foreach(ConfigNodePtr node, m_model){
		if(node->getName().startsWith("#")){
			max = node;
		}
	}

	if(max){
		Key k = max->getKey().dup();
		ckdb::elektraArrayIncName(k.getKey());
		return QString::fromStdString(k.getBaseName());
	}

	return "#0";
}

void TreeViewModel::refreshArrayNumbers()
{
	QList<ConfigNodePtr> arrayElements;

	foreach(ConfigNodePtr node, m_model){
		if(node->getName().startsWith("#")){
			arrayElements.append(node);
		}
	}

	if(!arrayElements.isEmpty()){

		arrayElements.at(0)->setName("#0");

		if(arrayElements.count() > 1){

			for(int i = 1; i < arrayElements.count(); i++){
				Key k = arrayElements.at(i - 1)->getKey().dup();
				ckdb::elektraArrayIncName(k.getKey());
				arrayElements.at(i)->setName(QString::fromStdString(k.getBaseName()));
			}
		}
	}
}

QStringList TreeViewModel::mountedBackends()
{
	KeySet keySet = collectCurrentKeySet();

	Backends::BackendInfoVector mtab = Backends::getBackendInfo(keySet);

	QStringList mountedBends;

	for (Backends::BackendInfoVector::const_iterator it = mtab.begin(); it != mtab.end(); ++it)
	{
		mountedBends.append(QString::fromStdString(it->name));
	}

	//cannot read the size of the QStringList in QML
	if (mountedBends.isEmpty())
		mountedBends.append("empty");

	return mountedBends;
}

QStringList TreeViewModel::getSplittedKeyname(const Key &key)
{
	QStringList names;

	for (Key::iterator i = key.begin(); i != key.end(); ++i)
	{
			names.append(QString::fromStdString(*i));
	}

	return names;
}

QHash<int, QByteArray> TreeViewModel::roleNames() const
{
	QHash<int, QByteArray> roles;

	roles[NameRole] = "name";
	roles[PathRole] = "path";
	roles[ValueRole] = "value";
	roles[ChildCountRole] = "childCount";
	roles[ChildrenRole] = "children";
	roles[ChildrenHaveNoChildrenRole] = "childrenHaveNoChildren";
	roles[MetaValueRole] = "metaValue";
	roles[NodeRole] = "node";
	roles[ParentModelRole] = "parentModel";
	roles[IndexRole] = "index";
	roles[IsNullRole] = "isNull";
	roles[IsExpandedRole] = "isExpanded";

	return roles;
}

void TreeViewModel::showConfigNodeMessage(QString title, QString text, QString detailedText)
{
	emit showMessage(title, text, detailedText);
}
