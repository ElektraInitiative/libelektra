#include "treeviewmodel.hpp"
#include "guibasickeyset.hpp"
#include <threewaymerge.hpp>
#include <automergeconfiguration.hpp>
#include <mergeconflictstrategy.hpp>
#include <automergestrategy.hpp>
#include <onesidestrategy.hpp>
#include <onesidevaluestrategy.hpp>
#include <toolexcept.hpp>
#include <backends.hpp>
#include <modules.hpp>
#include <plugin.hpp>
#include <plugins.hpp>
#include <kdbproposal.h> // for namespaces
#include <kdbconfig.h> // for DEBUG and VERBOSE

using namespace std;
using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::merging;

TreeViewModel::TreeViewModel(QObject* parentModel) :
	m_root("/", KEY_END),
	m_kdb(m_root)
{
	Q_UNUSED(parentModel);
}

TreeViewModel::TreeViewModel(const TreeViewModel& other)
	: QAbstractListModel()
{
	// copy from other
	m_model = other.m_model;
	m_root = other.m_root;
	m_kdb = other.m_kdb;
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
		if(node->getName() != modelData.toString()){
			node->setName(modelData.toString());
			node->setIsDirty(true);
		}
		break;

	case ValueRole:
		if(node->getValue() != modelData){
			node->setValue(modelData);
		}
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

void TreeViewModel::importConfiguration(const QString& name, const QString& format, QString& file, const QVariantList &mergeStrategies)
{
	Key		root(name.toStdString(), KEY_END);
	KeySet	originalKeys = collectCurrentKeySet();
	KeySet	base = originalKeys.cut(root);
	printWarnings (cerr, root);

	KeySet importedKeys;

	string formatString = format.toStdString();
	string fileString = file.remove("file://").toStdString();

	Modules modules;
	PluginPtr plugin = modules.load(formatString);

	Key errorKey (root);
	errorKey.setString (fileString);

	plugin->get(importedKeys, errorKey);

	stringstream ws;
	stringstream es;
	QString warnings;
	QString errors;

	printWarnings(ws, errorKey);
	warnings = QString::fromStdString(ws.str());
	printError(es, errorKey);
	errors  = QString::fromStdString(es.str());

	if(!errors.isEmpty())
	{
		emit showMessage(tr("Error"), tr("Failed to import configuration from %1 to %2.").arg(file, QString::fromStdString(root.getName())), errors);
		return;
	}

	ThreeWayMerge merger;

	foreach(QVariant s, mergeStrategies)
	{
		MergeConflictStrategy* strategy = getMergeStrategy(s.toString());

		if(strategy)
			merger.addConflictStrategy(strategy);
	}

	MergeResult result;

	try
	{
		result = merger.mergeKeySet(MergeTask (BaseMergeKeys (base, root), OurMergeKeys (base, root), TheirMergeKeys (importedKeys, root), root));
	}
	catch(...)//TODO: Which exceptions are possible?
	{
		emit showMessage(tr("Error"), tr("Could not merge keys."),"");
	}

	if (!result.hasConflicts ())
	{
		createNewNodes(result.getMergedKeys());

		if(importedKeys.size() > 0)
			emit showMessage(tr("Information"), tr("Successfully imported %1 keys.").arg(importedKeys.size()), "");
	}
	else
	{
		KeySet conflictSet = result.getConflictSet();
		QStringList conflicts;
		conflictSet.rewind();
		Key current;

		while ((current = conflictSet.next()))
		{
			QString ourConflict = QString::fromStdString(current.getMeta<string>("conflict/operation/our"));
			QString theirConflict = QString::fromStdString(current.getMeta<string>("conflict/operation/their"));

			conflicts.append(QString::fromStdString(current.getName()));
			conflicts.append("Ours: " + ourConflict + ", Theirs " + theirConflict);
			conflicts.append("\n");
		}

		emit showMessage(tr("Error"), tr("The were conflicts importing %1 (%2 format) into %3, no configuration was imported.").arg(file, format, name), conflicts.join("\n"));
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

	stringstream ws;
	stringstream es;
	QString warnings;
	QString errors;

	printWarnings(ws, errorKey);
	warnings = QString::fromStdString(ws.str());
	printError(es, errorKey);
	errors  = QString::fromStdString(es.str());

	if(errors.isEmpty() && !warnings.isEmpty())
		emit showMessage(tr("Information"), tr("Successfully exported configuration below %1 to %2, warnings were issued.").arg(QString::fromStdString(root.getName()), file), "");
	else if(!errors.isEmpty() && warnings.isEmpty())
		emit showMessage(tr("Error"), tr("Failed to export configuration below %1 to %2.").arg(QString::fromStdString(root.getName()), file), errors);
	else if(!errors.isEmpty() && !warnings.isEmpty())
		emit showMessage(tr("Error"), tr("Failed to export configuration below %1 to %2.").arg(QString::fromStdString(root.getName()), file), warnings + "\n" + errors);
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

	QQmlEngine::setObjectOwnership(searchResults, QQmlApplicationEngine::CppOwnership);

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

	if (node->hasChild(name) && !node->getChildByName(name)->isDirty())
	{
		sink(node->getChildByName(name), keys, key);
	}
	else
	{
		if(node->hasChild(name))
			node->getChildren()->removeRow(node->getChildIndexByName(name));

		ConfigNodePtr newNode;

		if (isLeaf)
			newNode = ConfigNodePtr(new ConfigNode(name, (node->getPath() + "/" + name), key, node->getChildren()));
		else
			newNode = ConfigNodePtr(new ConfigNode(name, (node->getPath() + "/" + name), NULL, node->getChildren()));

		node->appendChild(newNode);

		sink(newNode, keys, key);
	}
}

void TreeViewModel::populateModel()
{
	try
	{
		kdb::KeySet config;
		m_kdb.get(config, m_root);
		populateModel(config);
	}
	catch(kdb::KDBException const& e)
	{
		emit showMessage(QObject::tr("Error"), QObject::tr("Populating model failed, could not read from configuration."), e.what());
		throw;
	}
}

void TreeViewModel::populateModel(KeySet const & keySet)
{
	m_model.clear();

	using namespace ckdb; // for namespaces
	for (int i=KEY_NS_FIRST; i<=KEY_NS_LAST; ++i)
	{
		elektraNamespace ns = static_cast<elektraNamespace>(i);
		ConfigNodePtr toAdd;
		switch (ns)
		{
		case KEY_NS_SPEC:
			toAdd = ConfigNodePtr(new ConfigNode("spec", "spec", 0, this));
			break;
		case KEY_NS_PROC:
			// TODO: add generic commandline parsing
			break;
		case KEY_NS_DIR:
			toAdd = ConfigNodePtr(new ConfigNode("dir", "dir", 0, this));
			break;
		case KEY_NS_USER:
			toAdd = ConfigNodePtr(new ConfigNode("user", "user", 0, this));
			break;
		case KEY_NS_SYSTEM:
			toAdd = ConfigNodePtr(new ConfigNode("system", "system", 0, this));
			break;
		case KEY_NS_EMPTY:
			break;
		case KEY_NS_NONE:
			break;
		case KEY_NS_META:
			break;
		case KEY_NS_CASCADING:
			break;
		}
		if (toAdd) m_model << toAdd;
	}

	GUIBasicKeySet::setBasic(keySet);
	createNewNodes(keySet);
}

void TreeViewModel::createNewNodes(KeySet keySet)
{
	keySet.rewind();

	while (keySet.next())
	{
		Key k = keySet.current();
		QStringList keys = getSplittedKeyname(k);
		QString root = keys.takeFirst();

		for (int i = 0; i < m_model.count(); i++)
		{
			if (root == m_model.at(i)->getName())
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

namespace
{

#if DEBUG && VERBOSE
std::string printKey(Key const & k)
{
	std::string ret;
	ret += k.getName();

	if (ckdb::keyNeedSync(*k)) ret += "°";

	if (!k.isBinary())
	{
		ret += "=";
		ret += k.getString();
	}
	return ret;
}

void printKeys(KeySet const & theirs, KeySet const & base, KeySet const & ours)
{
	theirs.rewind();
	base.rewind();
	for (Key o : ours)
	{
		std::string prefix("user/guitest");
		Key t = theirs.next();
		Key b = base.next();
		if (!  ((o && !o.getName().compare(0, prefix.size(), prefix)) &&
			(t && !t.getName().compare(0, prefix.size(), prefix)) &&
			(b && !b.getName().compare(0, prefix.size(), prefix)))) continue;
		std::cout << printKey(o);;
		std::cout << "\t";
		!b.isValid() ? std::cout << "none" : std::cout << printKey(b);
		std::cout << "\t";
		!t.isValid() ? std::cout << "none" : std::cout << printKey(t);
		std::cout << std::endl;
	}
}
#endif

QStringList getConflicts(KeySet const & conflictSet)
{
	QStringList conflicts;
	conflictSet.rewind();
	Key current;

	while ((current = conflictSet.next()))
	{
		QString ourConflict = QString::fromStdString(current.getMeta<string>("conflict/operation/our"));
		QString theirConflict = QString::fromStdString(current.getMeta<string>("conflict/operation/their"));

		conflicts.append(QString::fromStdString(current.getName()));
		conflicts.append("Ours: " + ourConflict + ", Theirs " + theirConflict);
		conflicts.append("\n");
	}
	return conflicts;
}

KeySet handleConflict(KeySet const & theirs, KeySet const & ours)
{
	Key root("/", KEY_END);
	KeySet base = GUIBasicKeySet::basic();

	ThreeWayMerge merger;

	AutoMergeConfiguration configuration;
	configuration.configureMerger(merger);

#if DEBUG && VERBOSE
	std::cout << "guitest: handleConflict: before merge" << std::endl;
	printKeys(theirs, base, ours);
#endif

	MergeResult result = merger.mergeKeySet(MergeTask(BaseMergeKeys(base, root),
							  OurMergeKeys(ours, root),
							  TheirMergeKeys (theirs, root),
							  root));

#if DEBUG && VERBOSE
	std::cout << "guitest: handleConflict: after merge" << std::endl;
	printKeys(theirs, base, result.getMergedKeys());
#endif

	if (!result.hasConflicts ())
	{
		KeySet resultKeys = result.getMergedKeys();
		return resultKeys;
	}
	else
	{
		KeySet conflictSet = result.getConflictSet();
		QStringList conflicts = getConflicts(conflictSet);
		throw conflicts;
	}
}
}

void TreeViewModel::synchronize()
{
	KeySet ours = collectCurrentKeySet();

	try
	{
#if DEBUG && VERBOSE
		std::cout << "guitest: start" << std::endl;
		printKeys(ours, ours, ours);
#endif

		// write our config
		m_kdb.set(ours, m_root);
		// update our config (if no conflict)
		m_kdb.get(ours, m_root);

#if DEBUG && VERBOSE
		std::cout << "guitest: after get" << std::endl;
		printKeys(ours, ours, ours);
#endif

		GUIBasicKeySet::setBasic(ours);
		createNewNodes(ours);
	}
	catch (KDBException const&)
	{
		try
		{
			KeySet theirs = ours.dup();
			m_kdb.get(theirs, m_root);

			KeySet result = handleConflict(theirs, ours);

			// TODO: will rewrite everything because of current limitation in merger
			m_kdb.set(result, m_root);

#if DEBUG && VERBOSE
			std::cout << "guitest: exception: now after set" << std::endl;
			printKeys(theirs, result, ours);
#endif

			GUIBasicKeySet::setBasic(ours);
			createNewNodes(result);
		}
		catch (KDBException const& e)
		{
			emit showMessage(tr("Error"), tr("Synchronizing failed, could not write merged configuration."), e.what());
		}
		catch (QStringList const& conflicts)
		{
			emit showMessage(tr("Error"), tr("Synchronizing failed, conflicts occured."), conflicts.join("\n"));
		}
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

	emit updateIndicator();
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
		Key k = max->getKey();
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

void TreeViewModel::discardModel()
{
	delete this;
}

MergeConflictStrategy *TreeViewModel::getMergeStrategy(const QString &mergeStrategy)
{
	if(mergeStrategy == "Preserve")
		return new AutoMergeStrategy();
	else if(mergeStrategy == "Ours")
		return new OneSideStrategy(OURS);
	else if(mergeStrategy == "Theirs")
		return new OneSideStrategy(THEIRS);
	else if(mergeStrategy == "Base")
		return new OneSideStrategy(BASE);
	else if(mergeStrategy == "None")
		return NULL;

	return NULL;
}

void TreeViewModel::showConfigNodeMessage(QString title, QString text, QString detailedText)
{
	emit showMessage(title, text, detailedText);
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
