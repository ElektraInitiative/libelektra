#include "guisettings.hpp"
#include <QDebug>
#include <QWidget>
#include <QColor>

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

GUISettings::GUISettings(QObject *parentGUISettings) : QObject(parentGUISettings)
{
	QPalette palette;
	palette.setCurrentColorGroup(QPalette::Active);

	//initialize default colors
	m_highlightColor		= palette.highlight().color();
	m_frameColor			= palette.dark().color();
	m_nodeWithKeyColor		= palette.windowText().color();

	palette.setCurrentColorGroup(QPalette::Disabled);

	m_nodeWithoutKeyColor	= palette.windowText().color();

	//set base path for all colors
	m_base = "user/sw/libelektra.org/qt-gui/#0/";

	m_kdb.get(m_config, m_base);

	//check if stored colors exist, else create keys
	if(!lookup("highlightColor").isValid())
		append("highlightColor", m_highlightColor);
	else
		m_highlightColor = lookup("highlightColor");

	if(!lookup("frameColor").isValid())
		append("frameColor", m_frameColor);
	else
		m_frameColor = lookup("frameColor");

	if(!lookup("nodeWithKeyColor").isValid())
		append("nodeWithKeyColor", m_nodeWithKeyColor);
	else
		m_nodeWithKeyColor = lookup("nodeWithKeyColor");

	if(!lookup("nodeWithoutKeyColor").isValid())
		append("nodeWithoutKeyColor", m_nodeWithoutKeyColor);
	else
		m_nodeWithoutKeyColor = lookup("nodeWithoutKeyColor");

	//make sure there is a configuration, even on first start
	setKDB();
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
	append("highlightColor", color);

	emit highlightColorChanged();
}

void GUISettings::setFrameColor(const QColor &color)
{
	m_frameColor = color;
	append("frameColor", color);

	emit frameColorChanged();
}

void GUISettings::setNodeWithKeyColor(const QColor &color)
{
	m_nodeWithKeyColor = color;
	append("nodeWithKeyColor", color);

	emit nodeWithKeyColorChanged();
}

void GUISettings::setNodeWithoutKeyColor(const QColor &color)
{
	m_nodeWithoutKeyColor = color;
	append("nodeWithoutKeyColor", color);

	emit nodeWithoutKeyColorChanged();
}

void GUISettings::append(const QString &keyName, const QColor &color)
{
	m_config.append(Key(m_base + keyName.toStdString(), KEY_VALUE, color.name().toStdString().c_str(), KEY_END));
}

QColor GUISettings::lookup(const QString &keyName) const
{
	QColor color;

	try
	{
		color = m_config.lookup(m_base + keyName.toStdString()).get<QColor>();
	}
	catch(const KeyTypeConversion &ex)
	{
		qDebug() << ex.what();
	}

	return color;
}

void GUISettings::setKDB()
{
	try
	{
		m_kdb.set(m_config, m_base);
	}
	catch(const KDBException &ex)
	{
		qDebug() << ex.what();
	}
}
