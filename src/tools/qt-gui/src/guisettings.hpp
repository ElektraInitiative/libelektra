#ifndef GUISETTINGS_HPP
#define GUISETTINGS_HPP

#include <QObject>
#include <QColor>
#include <kdb.hpp>

/**
 * @brief The GUISettings class
 */

class GUISettings : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QColor highlightColor		READ highlightColor()		WRITE setHighlightColor(QColor)			NOTIFY highlightColorChanged())
	Q_PROPERTY(QColor frameColor			READ frameColor()			WRITE setFrameColor(QColor)				NOTIFY frameColorChanged())
	Q_PROPERTY(QColor nodeWithKeyColor		READ nodeWithKeyColor()		WRITE setNodeWithKeyColor(QColor)		NOTIFY nodeWithKeyColorChanged())
	Q_PROPERTY(QColor nodeWithoutKeyColor	READ nodeWithoutKeyColor()	WRITE setNodeWithoutKeyColor(QColor)	NOTIFY nodeWithoutKeyColorChanged())

public:
	/**
	 * @brief GUISettings
	 * @param parentGUISettings
	 */
	explicit GUISettings(QObject *parentGUISettings = 0);

	/**
	 * @brief GUISettings
	 * @param other
	 */
	GUISettings(GUISettings const& other) : QObject() {Q_UNUSED(other)}

	/**
	 * @brief highlightColor
	 * @return
	 */
	QColor				highlightColor() const;

	/**
	 * @brief frameColor
	 * @return
	 */
	QColor				frameColor() const;

	/**
	 * @brief nodeWithKeyColor
	 * @return
	 */
	QColor				nodeWithKeyColor() const;

	/**
	 * @brief nodeWithoutKeyColor
	 * @return
	 */
	QColor				nodeWithoutKeyColor() const;

	/**
	 * @brief setKDB
	 */
	Q_INVOKABLE	void	setKDB();

public slots:
	/**
	 * @brief setHighlightColor
	 * @param color
	 */
	void		setHighlightColor(const QColor &color);

	/**
	 * @brief setFrameColor
	 * @param color
	 */
	void		setFrameColor(const QColor &color);

	/**
	 * @brief setNodeWithKeyColor
	 * @param color
	 */
	void		setNodeWithKeyColor(const QColor &color);

	/**
	 * @brief setNodeWithoutKeyColor
	 * @param color
	 */
	void		setNodeWithoutKeyColor(const QColor &color);

signals:
	/**
	 * @brief highlightColorChanged
	 */
	void		highlightColorChanged();

	/**
	 * @brief frameColorChanged
	 */
	void		frameColorChanged();

	/**
	 * @brief nodeWithKeyColorChanged
	 */
	void		nodeWithKeyColorChanged();

	/**
	 * @brief nodeWithoutKeyColorChanged
	 */
	void		nodeWithoutKeyColorChanged();

private:
	kdb::KDB	m_kdb;
	kdb::KeySet m_config;
	std::string m_base;

	QColor		m_highlightColor;
	QColor		m_frameColor;
	QColor		m_nodeWithKeyColor;
	QColor		m_nodeWithoutKeyColor;

	QString		m_highlightColorString;
	QString		m_frameColorString;
	QString		m_nodeWKeyColorString;
	QString		m_nodeWOKeyColorString;

	/**
	 * @brief append
	 * @param keyName
	 * @param color
	 */
	void		append(const QString &keyName, const QColor &color);

	/**
	 * @brief lookupColor
	 * @param keyName
	 * @return
	 */
	QColor		lookupColor(const QString &keyName) const;
};

Q_DECLARE_METATYPE(GUISettings)

#endif // GUISETTINGS_HPP
