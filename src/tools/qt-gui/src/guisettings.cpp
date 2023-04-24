/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./guisettings.hpp"
#include <QDebug>
#include <QPalette>

using namespace kdb;

namespace kdb
{
template <>
inline QColor Key::get () const
{
	if (getStringSize () < 1)
	{
		throw KeyTypeConversion ();
	}

	std::string str = getString ();
	QColor c (str.c_str ());
	return c;
}
} // namespace kdb

void GUISettings::setDefaults ()
{
	QPalette palette;
	palette.setCurrentColorGroup (QPalette::Active);

	m_highlightColor = palette.highlight ().color ();
	m_frameColor = palette.dark ().color ();
	m_nodeWithKeyColor = palette.windowText ().color ();
	m_nodeWithoutKeyColor = palette.windowText ().color ();

	palette.setCurrentColorGroup (QPalette::Disabled);

	m_useSystemIconTheme = true;
	m_viewermode = false;
}

GUISettings::GUISettings (QObject * parentGUISettings)
: QObject (parentGUISettings), m_profile ("/current/"), m_base ("/sw/elektra/qtgui/#0/"), m_highlightColorString ("color/highlight"),
  m_frameColorString ("color/frame"), m_nodeWKeyColorString ("color/node/with"), m_nodeWOKeyColorString ("color/node/without"),
  m_useSystemIconThemeString ("icon/system"), m_legacyBase ("/sw/libelektra.org/qt-gui/#0/"),
  m_legacyHighlightColorString ("highlight_color"), m_legacyFrameColorString ("frame_color"),
  m_legacyNodeWKeyColorString ("node_with_key_color"), m_legacyNodeWOKeyColorString ("node_without_key_color"),
  m_viewermodeString ("mode/viewer")
{
	// initialize with hardcoded default colors
	setDefaults ();

	// check if stored colors exist, if so, load them
	getKDB ();
}

QColor GUISettings::highlightColor () const
{
	return m_highlightColor;
}

QColor GUISettings::frameColor () const
{
	return m_frameColor;
}

QColor GUISettings::nodeWithKeyColor () const
{
	return m_nodeWithKeyColor;
}

QColor GUISettings::nodeWithoutKeyColor () const
{
	return m_nodeWithoutKeyColor;
}


bool GUISettings::useSystemIconTheme () const
{
	return m_useSystemIconTheme;
}

bool GUISettings::viewermode () const
{
	return m_viewermode;
}

void GUISettings::setHighlightColor (const QColor & color)
{
	if (color != m_highlightColor)
	{
		m_highlightColor = color;
		appendColor (m_highlightColorString, color);

		emit highlightColorChanged ();
	}
}

void GUISettings::setFrameColor (const QColor & color)
{
	if (color != m_frameColor)
	{
		m_frameColor = color;
		appendColor (m_frameColorString, color);

		emit frameColorChanged ();
	}
}

void GUISettings::setNodeWithKeyColor (const QColor & color)
{
	if (color != m_nodeWithKeyColor)
	{
		m_nodeWithKeyColor = color;
		appendColor (m_nodeWKeyColorString, color);

		emit nodeWithKeyColorChanged ();
	}
}

void GUISettings::setNodeWithoutKeyColor (const QColor & color)
{
	if (color != m_nodeWithoutKeyColor)
	{
		m_nodeWithoutKeyColor = color;
		appendColor (m_nodeWOKeyColorString, color);

		emit nodeWithoutKeyColorChanged ();
	}
}

void GUISettings::useSystemIconTheme (const bool & use)
{
	if (use != m_useSystemIconTheme)
	{
		m_useSystemIconTheme = use;
		appendBool (m_useSystemIconThemeString, use);
		emit useSystemIconThemeChanged ();
	}
}

void GUISettings::setViewermode (bool vmode)
{
	if (vmode != m_viewermode)
	{
		m_viewermode = vmode;
		appendBool (m_viewermodeString, vmode);
		emit viewermodeChanged ();
	}
}

void GUISettings::appendColor (const std::string & keyName, const QColor & color)
{
	std::string name = "user:" + m_base + m_profile + keyName;
	m_config.append (Key (name, KEY_VALUE, color.name ().toStdString ().c_str (), KEY_END));
}

void GUISettings::appendBool (const std::string & keyName, const bool value)
{
	std::string name = "user:" + m_base + m_profile + keyName;
	Key key = m_config.lookup (name);
	if (key)
	{
		key.set<bool> (value);
	}
	else
	{
		Key k;
		k.setName (name);
		k.set<bool> (value);
		m_config.append (k);
	}
}

void GUISettings::lookupColor (const std::string & keyName, QColor & toSet) const
{
	Key key = m_config.lookup (keyName);

	if (!key) return; // nothing to do

	try
	{
		toSet = key.get<QColor> ();
	}
	catch (const KeyTypeConversion & ex)
	{
		qDebug () << ex.what ();
	}
}

void GUISettings::lookupBool (const std::string & keyName, bool & value) const
{
	Key key = m_config.lookup (keyName);

	if (!key) return; // nothing to do

	try
	{
		value = (key.get<bool> ());
	}
	catch (const KeyTypeConversion & ex)
	{
		qDebug () << ex.what ();
	}
}


void GUISettings::setKDB ()
{
	KDB kdb;
	KeySet dummySet; // cannot set without get

	try
	{
		kdb.get (dummySet, m_base);
	}
	catch (const KDBException & ex)
	{
		qDebug () << tr ("Could not read from database, unable to retrieve settings. The system responds: %1").arg (ex.what ());
	}

	// won't set config without user prefix
	try
	{
		kdb.set (m_config, "user:" + m_base);
	}
	catch (const KDBException & ex)
	{
		qDebug () << tr ("Could not write to database, unable to store settings. The system responds: %1").arg (ex.what ());
	}
}

void GUISettings::getKDB ()
{
	KDB kdb;

	// retrieve keys below base path
	try
	{
		kdb.get (m_config, m_legacyBase);
		kdb.get (m_config, m_base);
	}
	catch (const KDBException & ex)
	{
		qDebug () << tr ("Could not read from database, unable to retrieve settings. The system responds: %1").arg (ex.what ());
	}

	// (1) first with legacy:
	lookupColor (m_legacyBase + m_legacyHighlightColorString, m_highlightColor);
	lookupColor (m_legacyBase + m_legacyFrameColorString, m_frameColor);
	lookupColor (m_legacyBase + m_legacyNodeWKeyColorString, m_nodeWithKeyColor);
	lookupColor (m_legacyBase + m_legacyNodeWOKeyColorString, m_nodeWithoutKeyColor);

	for (int i = 0; i < 2; ++i)
	{
		std::string profile;
		switch (i)
		{
		case 0:
			profile = "%/";
			break; // (2) then with fallback profile
		case 1:
			profile = m_profile;
			break; // (3) and finally with current profile
		}
		lookupColor (m_base + profile + m_highlightColorString, m_highlightColor);
		lookupColor (m_base + profile + m_frameColorString, m_frameColor);
		lookupColor (m_base + profile + m_nodeWKeyColorString, m_nodeWithKeyColor);
		lookupColor (m_base + profile + m_nodeWOKeyColorString, m_nodeWithoutKeyColor);
		lookupBool (m_base + profile + m_useSystemIconThemeString, m_useSystemIconTheme);
		lookupBool (m_base + profile + m_viewermodeString, m_viewermode);
	}
}

void GUISettings::reset ()
{
	setDefaults ();
	setKDB ();
}
