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

	KDB kdb;
	KeySet keySet;

	try
	{
		kdb.get(keySet, name.toStdString());
	}
	catch (KDBException const& e)
	{
		emit showMessage(tr("Error"), tr("Could not read from configuration."), e.what());
	}

	createNewNodes(keySet);
}

void TreeViewModel::exportConfiguration(TreeViewModel* parentModel, int idx, QString format, QString file)
{
	synchronize();

	ConfigNodePtr node = parentModel->model().at(idx);

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
	ConfigNodePtr system(new ConfigNode("system", "system", 0, this));
	ConfigNodePtr user(new ConfigNode("user", "user", Key("user", KEY_END), this));

	m_model.clear();
	m_model << system << user;

	createNewNodes(keySet);
}

void TreeViewModel::createNewNodes(KeySet keySet)
{
	keySet.rewind();

	while (keySet.next())
	{
		Key k = keySet.current().dup();
		QString currentKey = QString::fromStdString(k.getName());
		QStringList keys = currentKey.split(qApp->property("KEY_DELIMITER").toRegularExpression());
		QString root = keys.takeFirst();

		if (root == "system")
		{
			sink(m_model.at(0), keys, k);
		}
		else if (root == "user")
		{
			sink(m_model.at(1), keys, k);
		}
		else
		{
			cerr << "TreeViewModel::populateModel: INVALID_KEY: " << currentKey.toStdString();
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
		kdb.set(keySet, "/user");
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
	const string keyName = string(Backends::mountpointsPath) + "/"  + backendName.toStdString();
	Key x(keyName, KEY_END);
	keySet.cut(x);

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
