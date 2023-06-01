/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./confignode.hpp"
#include "./treeviewmodel.hpp"

using namespace kdb;

ConfigNode::ConfigNode (QString name, QString path, const Key & key, TreeViewModel * parentModel)
: m_name (std::move (name)), m_path (std::move (path)), m_key (key), m_children (new TreeViewModel), m_metaData (nullptr),
  m_parentModel (parentModel), m_isExpanded (false), m_isDirty (false)
{
	setValue ();

	if (m_key)
	{
		m_metaData = new TreeViewModel;
		populateMetaModel ();
	}

	connect (m_children, SIGNAL (expandNode (bool)), this, SLOT (setIsExpanded (bool)));
	connect (this, SIGNAL (showMessage (QString, QString, QString)), parentModel,
		 SLOT (showConfigNodeMessage (QString, QString, QString)));
}

ConfigNode::ConfigNode (const ConfigNode & other)
: QObject (), m_name (other.m_name), m_path (other.m_path), m_value (other.m_value), m_key (other.m_key.dup ()),
  m_children (new TreeViewModel), m_metaData (nullptr), m_parentModel (nullptr), m_isExpanded (other.m_isExpanded), m_isDirty (false)
{
	if (other.m_children)
	{
		foreach (ConfigNodePtr node, other.m_children->model ())
		{
			m_children->append (ConfigNodePtr (new ConfigNode (*node)));
		}
	}

	if (other.m_metaData)
	{
		m_metaData = new TreeViewModel;
		foreach (ConfigNodePtr node, other.m_metaData->model ())
		{
			m_metaData->append (ConfigNodePtr (new ConfigNode (*node)));
		}
	}

	connect (m_children, SIGNAL (expandNode (bool)), this, SLOT (setIsExpanded (bool)));
}

ConfigNode::ConfigNode () : m_children (nullptr), m_metaData (nullptr), m_parentModel (nullptr), m_isExpanded (false), m_isDirty (false)
{
	// this constructor is used to create metanodes
}

ConfigNode::~ConfigNode ()
{
	delete m_children;
	delete m_metaData;
}

int ConfigNode::getChildCount () const
{
	if (m_children) return m_children->rowCount ();
	return 0;
}

QString ConfigNode::getName () const
{
	return m_name;
}

QString ConfigNode::getPath () const
{
	return m_path;
}

QVariant ConfigNode::getValue () const
{
	return m_value;
}

void ConfigNode::setName (const QString & name)
{
	int index = m_path.lastIndexOf ("/");

	if (index != -1)
	{
		m_path.replace (index, m_path.length () - index, "/" + name);
	}

	if (!m_key)
		m_key = Key (m_path.toStdString (), KEY_END);
	else
		m_key = m_key.dup ();

	try
	{
		if (m_key.getBaseName ().compare (name.toStdString ()) != 0) m_key.setBaseName (name.toStdString ());
	}
	catch (KeyInvalidName const & ex)
	{
		emit showMessage (tr ("Error"), tr ("Could not set name because Keyname \"%1\" is invalid.").arg (name), ex.what ());
		return;
	}

	m_name = name;
}

void ConfigNode::setValue (const QVariant & value)
{
	if (!m_key)
		m_key = Key (m_path.toStdString (), KEY_END);
	else
		m_key = m_key.dup ();

	try
	{
		if (m_key.getString ().compare (value.toString ().toStdString ()) != 0)
		{
			m_key.setString (value.toString ().toStdString ());
			m_value = value;
		}
	}
	catch (KeyTypeMismatch const & ex)
	{
		emit showMessage (tr ("Error"), tr ("Creating/Editing binary keys is not yet supported."), ex.what ());
		return;
	}
}


void ConfigNode::setMeta (const QString & name, const QVariant & value)
{
	if (!m_key)
		m_key = Key (m_path.toStdString (), KEY_END);
	else
		m_key = m_key.dup ();

	m_key.setMeta (name.toStdString (), value.toString ().toStdString ());
	m_name = name;
	m_value = value;
}


void ConfigNode::setMeta (const QVariantMap & metaData)
{
	if (m_metaData)
	{
		// delete old metadata in key
		for (auto & elem : m_metaData->model ())
		{
			elem->deleteMeta (elem->getName ());
		}
		// delete old metadata in model
		m_metaData->clearMetaModel ();
	}
	else
		m_metaData = new TreeViewModel;

	// create new metadata nodes in model
	for (int i = 0; i < metaData.size (); i++)
	{
		m_metaData->insertMetaRow (i, m_key, m_name);
	}

	int counter = 0;
	// set new metadata
	for (QVariantMap::const_iterator iter = metaData.begin (); iter != metaData.end (); iter++)
	{
		QVariantList tmp;
		tmp << iter.key () << iter.value ();
		m_metaData->setData (m_metaData->index (counter), tmp, TreeViewModel::MetaValueRole);
		counter++;
	}
}

void ConfigNode::deleteMeta (const QString & name)
{
	if (m_key) m_key.delMeta (name.toStdString ());
}

void ConfigNode::accept (Visitor & visitor)
{
	visitor.visit (*this);

	if (m_children)
	{
		foreach (ConfigNodePtr node, m_children->model ())
			node->accept (visitor);
	}
}

Key ConfigNode::getKey () const
{
	return m_key;
}

int ConfigNode::getChildIndexByName (const QString & name)
{
	if (m_children)
	{
		for (int i = 0; i < m_children->rowCount (); i++)
		{
			if (m_children->model ().at (i)->getName () == name) return i;
		}
	}

	return -1;
}

TreeViewModel * ConfigNode::getParentModel ()
{
	return m_parentModel;
}

void ConfigNode::setParentModel (TreeViewModel * parentModel)
{
	m_parentModel = parentModel;
}

bool ConfigNode::isExpanded () const
{
	return m_isExpanded;
}
bool ConfigNode::isDirty () const
{
	return m_isDirty;
}

void ConfigNode::setIsDirty (bool dirty)
{
	m_isDirty = dirty;
}

void ConfigNode::updateNode (Key key)
{
	m_key = key;
	setValue ();
	populateMetaModel ();
}

void ConfigNode::setIsExpanded (bool value)
{
	m_isExpanded = value;
}

bool ConfigNode::isNamespaceRoot () const
{
	return m_path.indexOf ('/') < 0;
}

void ConfigNode::populateMetaModel ()
{
	if (m_key)
	{
		m_metaData->clearMetaModel ();

		ckdb::KeySet * metaKeys = ckdb::keyMeta (m_key.getKey ());
		for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
		{
			Key curMeta (ckdb::ksAtCursor (metaKeys, it));
			ConfigNodePtr node (new ConfigNode ());

			node->setName (QString::fromStdString (m_key.getName ()));
			node->setKey (m_key);
			node->setMeta (QString::fromStdString (curMeta.getName ()),
				       QVariant::fromValue (QString::fromStdString (curMeta.getString ())));

			m_metaData->insertRow (m_metaData->rowCount (), node, false);
		}
	}
}

void ConfigNode::setValue ()
{
	if (m_key && m_key.isString ())
		m_value = QVariant::fromValue (QString::fromStdString (m_key.getString ()));
	else if (m_key && m_key.isBinary ())
		m_value = QVariant::fromValue (QString::fromStdString (m_key.getBinary ()));
}

void ConfigNode::setKey (Key key)
{
	m_key = key;
}

void ConfigNode::setKeyName (const QString & name)
{
	if (m_key)
	{
		try
		{
			m_key.setName (name.toStdString ());
		}
		catch (KeyInvalidName const & ex)
		{
			emit showMessage (tr ("Error"), tr ("Could not set name because Keyname \"%1\" is invalid.").arg (name),
					  ex.what ());
			return;
		}
	}
}

void ConfigNode::appendChild (ConfigNodePtr node)
{
	m_children->append (node);
}

bool ConfigNode::hasChild (const QString & name) const
{
	if (m_children)
	{
		foreach (ConfigNodePtr node, m_children->model ())
		{
			if (node->getName () == name)
			{
				return true;
			}
		}
	}

	return false;
}

TreeViewModel * ConfigNode::getChildren () const
{
	return m_children;
}

TreeViewModel * ConfigNode::getMetaKeys () const
{
	return m_metaData;
}

ConfigNodePtr ConfigNode::getChildByName (QString & name) const
{
	if (m_children)
	{
		foreach (ConfigNodePtr node, m_children->model ())
		{
			if (node->getName () == name)
			{
				return node;
			}
		}
	}

	return ConfigNodePtr ();
}

ConfigNodePtr ConfigNode::getChildByIndex (int index) const
{
	if (m_children)
	{
		if (index >= 0 && index < m_children->model ().length ()) return m_children->model ().at (index);
	}

	return ConfigNodePtr ();
}

void ConfigNode::setPath (const QString & path)
{
	m_path = path;
	setKeyName (path);

	if (m_children)
	{
		foreach (ConfigNodePtr node, m_children->model ())
		{
			node->setPath (m_path + "/" + node->getName ());
		}
	}
}

bool ConfigNode::childrenHaveNoChildren () const
{
	int childcount = 0;

	if (m_children)
	{
		foreach (ConfigNodePtr node, m_children->model ())
		{
			childcount += node->getChildCount ();
		}
	}

	return childcount == 0;
}
