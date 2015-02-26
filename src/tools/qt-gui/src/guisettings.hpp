#ifndef GUISETTINGS_HPP
#define GUISETTINGS_HPP

#include <QObject>
#include <QColor>
#include <kdb.hpp>

class GUISettings : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QColor highlightColor		READ highlightColor()		WRITE setHighlightColor(QColor)			NOTIFY highlightColorChanged())
	Q_PROPERTY(QColor frameColor			READ frameColor()			WRITE setFrameColor(QColor)				NOTIFY frameColorChanged())
	Q_PROPERTY(QColor nodeWithKeyColor		READ nodeWithKeyColor()		WRITE setNodeWithKeyColor(QColor)		NOTIFY nodeWithKeyColorChanged())
	Q_PROPERTY(QColor nodeWithoutKeyColor	READ nodeWithoutKeyColor()	WRITE setNodeWithoutKeyColor(QColor)	NOTIFY nodeWithoutKeyColorChanged())

public:
	explicit GUISettings(QObject *parentGUISettings = 0);
	GUISettings(GUISettings const& other) : QObject() {Q_UNUSED(other)}

	QColor				highlightColor() const;
	QColor				frameColor() const;
	QColor				nodeWithKeyColor() const;
	QColor				nodeWithoutKeyColor() const;
	Q_INVOKABLE	void	setKDB();

public slots:
	void		setHighlightColor(const QColor &color);
	void		setFrameColor(const QColor &color);
	void		setNodeWithKeyColor(const QColor &color);
	void		setNodeWithoutKeyColor(const QColor &color);

signals:
	void		highlightColorChanged();
	void		frameColorChanged();
	void		nodeWithKeyColorChanged();
	void		nodeWithoutKeyColorChanged();

private:
	kdb::KDB	m_kdb;
	kdb::KeySet m_config;
	std::string m_base;

	QColor		m_highlightColor;
	QColor		m_frameColor;
	QColor		m_nodeWithKeyColor;
	QColor		m_nodeWithoutKeyColor;

	void		append(const QString &keyName, const QColor &color);
	QColor		lookup(const QString &keyName) const;
};

Q_DECLARE_METATYPE(GUISettings)

#endif // GUISETTINGS_HPP
