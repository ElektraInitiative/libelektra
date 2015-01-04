#include "treeviewmodel.hpp"
#include <factory.hpp>
#include <command.hpp>
#include <cmdline.hpp>
#include <external.hpp>
#include <toolexcept.hpp>
#include <backends.hpp>
#include <kdbprivate.h>
#include <modules.hpp>
#include <plugin.hpp>
#include <plugins.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

TreeViewModel::TreeViewModel(QObject* parent)
{
	Q_UNUSED(parent);
}

TreeViewModel::TreeViewModel(const TreeViewModel& other)
	: QAbstractListModel()
{
	m_model = other.m_model; // copy from other list
}

int TreeViewModel::rowCount(const QModelIndex& parent) const
{
	Q_UNUSED(parent);
	return m_model.count();
}

QVariant TreeViewModel::data(const QModelIndex& index, int role) const
{
	if (!index.isValid())
	{
		emit showMessage(tr("Error"), tr("Index not valid."), QString("TreeViewModel::data: Index = %1,\nModel size = %2").arg(index.row()).arg(m_model.count()));
		return QVariant();
	}

	if (index.row() > (m_model.size() - 1))
	{
		emit showMessage(tr("Error"), QString(tr("Index too high. ")), QString("TreeViewModel::data: Index: %1").arg(index.row()));
		return QVariant();
	}

	ConfigNodePtr node = m_model.at(index.row());

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
		return QVariant::fromValue(index.row());

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

bool TreeViewModel::setData(const QModelIndex& index, const QVariant& data, int role)
{
	if (!index.isValid() || index.row() > (m_model.size() - 1))
	{
		emit showMessage(tr("Error"), tr("Index not valid."), QString("TreeViewModel::setData: Index = %1,\nModel size = %2").arg(index.row()).arg(m_model.count()));
		return false;
	}

	ConfigNodePtr node = m_model.at(index.row());

	switch (role)
	{

	case NameRole:
		node->setName(data.toString());
		break;

	case ValueRole:
		node->setValue(data);
		break;

	case MetaValueRole:
	{
		QVariantList valueList = data.toList();
		node->setMeta(valueList.at(0).toString(), valueList.at(1));
		break;
	}

	case IsExpandedRole:
		node->setIsExpanded(data.toBool());
	}

	emit dataChanged(index, index);

	return true;
}

// TODO: Why are there two implementations of setData needed?
// Because QML cannot call setData() directly (see https://bugreports.qt-project.org/browse/QTBUG-7932)
// and to make it possible to set data without a QModelIndex
void TreeViewModel::setData(int index, const QVariant& value, const QString& role)
{
	if (index < 0 || index > m_model.size() - 1)
	{
		emit showMessage(tr("Error"), tr("Index not valid."), QString("TreeViewModel::setData: Index = %1, Model size = %2").arg(index).arg(m_model.size()));
		return;
	}

	QModelIndex modelIndex = this->index(index);

	if (role == "Name")
	{
		setData(modelIndex, value, NameRole);
	}
	else if (role == "Value")
	{
		setData(modelIndex, value, ValueRole);
	}
	else if (role == "MetaValue")
	{
		setData(modelIndex, value, MetaValueRole);
	}
	else if (role == "isExpanded")
	{
		setData(modelIndex, value, IsExpandedRole);
	}
	else
		return;
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
	synchronize();

	file.remove("file://");

	Factory f;

	QByteArray executable = QString("kdb").toLocal8Bit();
	QByteArray commandName = QString("import").toLocal8Bit();
	QByteArray importName = name.toLocal8Bit();
	QByteArray importFormat = format.toLocal8Bit();
	QByteArray importFile = file.toLocal8Bit();
	QByteArray importMergeStrategy = QString("-s" + mergeStrategy).toLocal8Bit();

	char* argv[] = {executable.data(), commandName.data(), importName.data(), importFormat.data(), importFile.data(), importMergeStrategy.data()};
	int argc = sizeof(argv) / sizeof(char*) - 1;

	string command = argv[1];

	try
	{
		CommandPtr cmd = f.get(command);

		Cmdline cl(argc, argv, cmd.get());

		try
		{
			cmd->execute(cl);
		}
		catch (invalid_argument const& ia)
		{
			emit showMessage(tr("Error"), tr("Importing the configuration from file failed because there were invalid arguments passed."), QString(ia.what()));
		}
	}
	catch (CommandException const& ce)
	{
		emit showMessage(tr("Error"), tr("Importing the configuration from file failed because of a faulty command."), QString(ce.what()));
	}
	catch (Key& key)
	{
		stringstream ws;
		stringstream es;

		ws << printWarnings(cerr, key);
		es << printError(cerr, key);

		emit showMessage(tr("Error"), tr("Importing the configuration from file failed while accessing the key database."), QString::fromStdString(ws.str()) + QString::fromStdString(es.str()));
	}
	catch (exception const& ce)
	{
		emit showMessage(tr("Error"), tr("Importing the configuration from file terminated unsuccessfully."), QString(ce.what()));
	}
	catch (...)
	{
		emit showMessage(tr("Error"), tr("Unknown error"), "TreeViewModel::importConfiguration");
	}

	getFromKdb();

	populateModel();
}

void TreeViewModel::exportConfiguration(TreeViewModel* model, int index, QString format, QString file)
{
	synchronize();

	ConfigNodePtr node = model->model().at(index);

	file.remove("file://");

	Factory f;

	QByteArray executable = QString("kdb").toLocal8Bit();
	QByteArray commandName = QString("export").toLocal8Bit();
	QByteArray exportName = node->getPath().toLocal8Bit();
	QByteArray exportFormat = format.toLocal8Bit();
	QByteArray exportFile = file.toLocal8Bit();

	char* argv[] = {executable.data(), commandName.data(), exportName.data(), exportFormat.data(), exportFile.data(), NULL};
	int argc = sizeof(argv) / sizeof(char*) - 1;

	string command = argv[1];

	try
	{
		CommandPtr cmd = f.get(command);

		Cmdline cl(argc, argv, cmd.get());

		try
		{
			cmd->execute(cl);
		}
		catch (invalid_argument const& ia)
		{
			emit showMessage(tr("Error"), tr("Exporting the configuration to file failed because there were invalid arguments passed."), QString(ia.what()));
			return;
		}
	}
	catch (CommandException const& ce)
	{
		emit showMessage(tr("Error"), tr("Exporting the configuration to file terminated unsuccessfully."), QString(ce.what()));
		return;
	}
	catch (Key& key)
	{
		stringstream ws;
		stringstream es;

		ws << printWarnings(cerr, key);
		es << printError(cerr, key);

		emit showMessage(tr("Error"), tr("Exporting the configuration to file failed while accessing the key database."), QString::fromStdString(ws.str()) + QString::fromStdString(es.str()));
		return;
	}
	catch (exception const& ce)
	{
		emit showMessage(tr("Error"), tr("Exporting the configuration to file terminated unsuccessfully."), QString(ce.what()));
		return;
	}
	catch (...)
	{
		emit showMessage(tr("Error"), tr("Unknown error."), "TreeViewModel::exportConfiguration");
	}
}

void TreeViewModel::setKeySet(const KeySet& set)
{
	m_keySet = set;
}

KeySet TreeViewModel::getKeySet()
{
	return m_keySet;
}

void TreeViewModel::collectCurrentKeySet()
{
	KeySetVisitor ksVisit;
	accept(ksVisit);

	setKeySet(ksVisit.getKeySet());
}

Qt::ItemFlags TreeViewModel::flags(const QModelIndex& index) const
{
	if (!index.isValid())
		return Qt::ItemIsEnabled;

	return QAbstractItemModel::flags(index) | Qt::ItemIsEditable | Qt::ItemIsDragEnabled | Qt::ItemIsDropEnabled;
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

	foreach (ConfigNodePtr node, m_model)
	{
		find(node, searchResults, term);
	}

	if (searchResults->model().count() == 0)
	{
		searchResults->model().append(ConfigNodePtr(new ConfigNode("NotfoundNode", tr("There were no results matching your query."), 0, this)));
	}

	return QVariant::fromValue(searchResults);
}

void TreeViewModel::find(ConfigNodePtr node, TreeViewModel* searchResults, const QString term)
{
	int tmpChildCount = node->getChildCount();

	if (tmpChildCount > 0)
	{
		for (int i = 0; i < tmpChildCount; i++)
		{
			find(node->getChildByIndex(i), searchResults, term);
		}
	}

	if (node->getName().contains(term) || node->getValue().toString().contains(term))
	{
		searchResults->model().append(node);
	}
}

bool TreeViewModel::removeRow(int row, const QModelIndex& parent)
{
	Q_UNUSED(parent);

	if (row < 0 || row > m_model.size() - 1)
	{
		emit showMessage(tr("Error"), tr("Index not valid."), QString("TreeViewModel::removeRow: Index = %1, Model size = %2").arg(m_model.size()).arg(row));
		return false;
	}

	beginRemoveRows(QModelIndex(), row, row);

	if (!m_model.isEmpty())
		m_model.takeAt(row);

	endRemoveRows();

	int childCount = 0;

	foreach (ConfigNodePtr node, m_model)
	{
		childCount += node->getChildCount();
	}

	if (childCount == 0)
		emit expandNode(false);

	return true;
}

bool TreeViewModel::insertRow(int row, const QModelIndex& parent)
{
	Q_UNUSED(parent);
	ConfigNodePtr node(new ConfigNode);
	node->setName(QString::fromStdString(m_metaModelParent.getName()));
	node->setKey(m_metaModelParent);
	node->setParentModel(this);

	beginInsertRows(QModelIndex(), row, row);
	m_model.insert(row, node);
	endInsertRows();

	return true;
}

void TreeViewModel::insertRow(int row, ConfigNodePtr node)
{
	beginInsertRows(QModelIndex(), row, row);
	node->setParentModel(this);
	m_model.insert(row, node);
	endInsertRows();
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

void TreeViewModel::sink(ConfigNodePtr node, QStringList keys, QString path, const Key& key)
{
	if (keys.length() == 0)
		return;

	bool isLeaf = (keys.length() == 1);

	QString name =  keys.takeFirst();

	if (node->hasChild(name))
	{
		sink(node->getChildByName(name), keys, node->getPath() + "/" + name, key);
	}
	else
	{
		ConfigNodePtr newNode;

		if (isLeaf)
			newNode = ConfigNodePtr(new ConfigNode(name, (path + "/" + name), key, node->getChildren()));
		else
			newNode = ConfigNodePtr(new ConfigNode(name, (path + "/" + name), NULL, node->getChildren()));

		node->appendChild(newNode);

		sink(newNode, keys, node->getPath() + "/" + name, key);
	}
}

void TreeViewModel::populateModel()
{
	ConfigNodePtr system(new ConfigNode("system", "system", 0, this));
	ConfigNodePtr user(new ConfigNode("user", "user", Key("user", KEY_END), this));

	m_model.clear();
	m_model << system << user;

	m_keySet.rewind();

	int i = 0;
	double s = 100/(double) m_keySet.size();

	while (m_keySet.next())
	{
		Key k = m_keySet.current().dup();
		QString currentKey = QString::fromStdString(k.getName());
		QStringList keys = currentKey.split("/");
		QString root = keys.takeFirst();

		if (root == "system")
		{
			sink(m_model.at(0), keys, "system", k);
		}
		else if (root == "user")
		{
			sink(m_model.at(1), keys, "user", k);
		}
		else
		{
			cerr << "TreeViewModel::populateModel: INVALID_KEY: " << currentKey.toStdString();
		}

		++i;
		emit updateProgress(s*i);

	}

	emit finished();
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
	getFromKdb();

	collectCurrentKeySet();

	setToKdb();
}

void TreeViewModel::clearMetaModel()
{
	beginResetModel();
	m_model.clear();
	endResetModel();
}

void TreeViewModel::unMountBackend(QString backendName)
{
	getFromKdb();

	const string keyName = string(Backends::mountpointsPath) + "/"  + backendName.toStdString();
	Key x(keyName, KEY_END);
	m_keySet.cut(x);

	TreeViewModel *mountPoint;

	QStringList path = QString::fromStdString(keyName).split("/");
	path.removeLast();

	ConfigNodePtr node;

	QString root = path.takeFirst();

	if(root == "system")
		node = m_model.at(0);
	else if(root == "user")
		node = m_model.at(1);

	while(path.length() > 0){
		QString name = path.takeFirst();
		node = node->getChildByName(name);
	}

	mountPoint = node->getChildren();

	mountPoint->removeRow(node->getChildIndexByName(backendName));

	setToKdb();
}

void TreeViewModel::refresh()
{
	QList<ConfigNodePtr> newModel(m_model);

	beginResetModel();
	m_model.clear();

	foreach (ConfigNodePtr node, newModel)
	{
		m_model.append(node);
	}

	endResetModel();
}

int TreeViewModel::count() const
{
	return m_model.count();
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
	collectCurrentKeySet();

	Backends::BackendInfoVector mtab = Backends::getBackendInfo(m_keySet);

	QStringList mountedBackends;

	for (Backends::BackendInfoVector::const_iterator it = mtab.begin(); it != mtab.end(); ++it)
	{
		mountedBackends.append(QString::fromStdString(it->name));
	}

	//cannot read the size of the QStringList in QML
	if (mountedBackends.isEmpty())
		mountedBackends.append("empty");

	return mountedBackends;
}

void TreeViewModel::setToKdb()
{
	try
	{
		m_kdb.set(m_keySet, "/");
	}
	catch (KDBException const& e)
	{
		emit showMessage(tr("Error"), tr("Synchronizing failed."), e.what());
	}
}

void TreeViewModel::getFromKdb()
{
	try
	{
		m_kdb.get(m_keySet, "/");
	}
	catch (KDBException const& e)
	{
		emit showMessage(tr("Error"), tr("Could not read from configuration."), e.what());
	}
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
