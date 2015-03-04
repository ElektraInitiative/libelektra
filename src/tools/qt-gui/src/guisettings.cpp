#include "guisettings.hpp"
#include <QDebug>
#include <QPalette>

using namespace kdb;

namespace kdb
{
template <>
inline QColor Key::get() const
{
	if (getStringSize() < 1)
	{
		throw KeyTypeConversion();
	}

	std::string str = getString();
	QColor c(str.c_str());
	return c;
}
}

GUISettings::GUISettings(QObject *parentGUISettings)
	: QObject(parentGUISettings)
	, m_base("/sw/libelektra.org/qt-gui/#0/")
	, m_highlightColorString("highlight_color")
	, m_frameColorString("frame_color")
	, m_nodeWKeyColorString("node_with_key_color")
	, m_nodeWOKeyColorString("node_without_key_color")
{
	QPalette palette;
	palette.setCurrentColorGroup(QPalette::Active);

	//initialize default colors
	m_highlightColor		= palette.highlight().color();
	m_frameColor			= palette.dark().color();
	m_nodeWithKeyColor		= palette.windowText().color();

	palette.setCurrentColorGroup(QPalette::Disabled);

	m_nodeWithoutKeyColor	= palette.windowText().color();

	//retrieve keys below base path
	try
	{
		m_kdb.get(m_config, m_base);
	}
	catch(const KDBException &ex)
	{
		qDebug() << ex.what();
	}

	//check if stored colors exist, if so, load them,else create them
	if(!lookupColor(m_highlightColorString).isValid())
		append(m_highlightColorString, m_highlightColor);
	else
		m_highlightColor = lookupColor(m_highlightColorString);

	if(!lookupColor(m_frameColorString).isValid())
		append(m_frameColorString, m_frameColor);
	else
		m_frameColor = lookupColor(m_frameColorString);

	if(!lookupColor(m_nodeWKeyColorString).isValid())
		append(m_nodeWKeyColorString, m_nodeWithKeyColor);
	else
		m_nodeWithKeyColor = lookupColor(m_nodeWKeyColorString);

	if(!lookupColor(m_nodeWOKeyColorString).isValid())
		append(m_nodeWOKeyColorString, m_nodeWithoutKeyColor);
	else
		m_nodeWithoutKeyColor = lookupColor(m_nodeWOKeyColorString);
}

QColor GUISettings::highlightColor() const
{
	return m_highlightColor;
}

QColor GUISettings::frameColor() const
{
	return m_frameColor;
}

QColor GUISettings::nodeWithKeyColor() const
{
	return m_nodeWithKeyColor;
}

QColor GUISettings::nodeWithoutKeyColor() const
{
	return m_nodeWithoutKeyColor;
}

void GUISettings::setHighlightColor(const QColor &color)
{
	m_highlightColor = color;
	append(m_highlightColorString, color);

	emit highlightColorChanged();
}

void GUISettings::setFrameColor(const QColor &color)
{
	m_frameColor = color;
	append(m_frameColorString, color);

	emit frameColorChanged();
}

void GUISettings::setNodeWithKeyColor(const QColor &color)
{
	m_nodeWithKeyColor = color;
	append(m_nodeWKeyColorString, color);

	emit nodeWithKeyColorChanged();
}

void GUISettings::setNodeWithoutKeyColor(const QColor &color)
{
	m_nodeWithoutKeyColor = color;
	append(m_nodeWOKeyColorString, color);

	emit nodeWithoutKeyColorChanged();
}

void GUISettings::append(const QString &keyName, const QColor &color)
{
	m_config.append(Key("user" + m_base + keyName.toStdString(), KEY_VALUE, color.name().toStdString().c_str(), KEY_END));
}

QColor GUISettings::lookupColor(const QString &keyName) const
{
	QColor	color;
	Key		key = m_config.lookup(m_base + keyName.toStdString());

	if(!key)
		return color;

	try
	{
		color = key.get<QColor>();
	}
	catch(const KeyTypeConversion &ex)
	{
		qDebug() << ex.what();
	}

	return color;
}

void GUISettings::setKDB()
{
	//won't set config without user prefix
	try
	{
		m_kdb.set(m_config, "user" + m_base);
	}
	catch(const KDBException &ex)
	{
		qDebug() << ex.what();
	}
}
