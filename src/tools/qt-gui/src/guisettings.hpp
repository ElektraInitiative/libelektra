/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef GUISETTINGS_HPP
#define GUISETTINGS_HPP

#include <QColor>
#include <QObject>
#include <kdb.hpp>

/**
 * @brief The GUISettings class. It manages the settings of the GUI. At the moment the only settings are colorsettings.
 */
class GUISettings : public QObject
{
	Q_OBJECT

	Q_PROPERTY (QColor highlightColor READ highlightColor () WRITE setHighlightColor (QColor) NOTIFY highlightColorChanged ())
	Q_PROPERTY (QColor frameColor READ frameColor () WRITE setFrameColor (QColor) NOTIFY frameColorChanged ())
	Q_PROPERTY (QColor nodeWithKeyColor READ nodeWithKeyColor () WRITE setNodeWithKeyColor (QColor) NOTIFY nodeWithKeyColorChanged ())
	Q_PROPERTY (QColor nodeWithoutKeyColor READ nodeWithoutKeyColor () WRITE setNodeWithoutKeyColor (QColor)
			    NOTIFY nodeWithoutKeyColorChanged ())
	Q_PROPERTY (bool useSystemIconTheme READ useSystemIconTheme () WRITE useSystemIconTheme (bool) NOTIFY useSystemIconThemeChanged ())
	Q_PROPERTY (bool viewermode READ viewermode () WRITE setViewermode (bool) NOTIFY viewermodeChanged ())

public:
	/**
	 * @brief GUISettings The default constructor
	 * @param parentGUISettings
	 */
	explicit GUISettings (QObject * parentGUISettings = nullptr);

	/**
	 * @brief GUISettings The copy contructor. Mandatory.
	 * @param other
	 */
	GUISettings (GUISettings const & other) : QObject ()
	{
		Q_UNUSED (other)
	}

	/**
	 * @brief highlightColor The color of the highlight bar in the views.
	 * @return The color of the highlight bar in the views.
	 */
	QColor highlightColor () const;

	/**
	 * @brief frameColor The color of the window frames in the views.
	 * @return The color of the window frames in the views.
	 */
	QColor frameColor () const;

	/**
	 * @brief nodeWithKeyColor The color of the ConfigNodes that contain a kdb::Key
	 * @return The color of the ConfigNodes that contain a kdb::Key
	 */
	QColor nodeWithKeyColor () const;

	/**
	 * @brief nodeWithoutKeyColor The color of the ConfigNodes that do not contain a kdb::Key
	 * @return The color of the ConfigNodes that do not contain a kdb::Key
	 */
	QColor nodeWithoutKeyColor () const;

	/**
	 * @brief if the system icon theme should be used instead of qt guis own icon set
	 * @return true if the system icon theme should be used
	 */
	bool useSystemIconTheme () const;

	/**
	 * @brief setKDB Makes the current color settings permanent.
	 */
	Q_INVOKABLE void setKDB ();

	/**
	 * @brief reset Deletes all permanent keys and restores the colors to the default values.
	 */
	Q_INVOKABLE void reset ();

	bool viewermode () const;

public slots:
	/**
	 * @brief setHighlightColor Sets the new highlight color.
	 * @param color The new highlight color.
	 */
	void setHighlightColor (const QColor & color);

	/**
	 * @brief setFrameColor Sets the new frame color.
	 * @param color The new frame color.
	 */
	void setFrameColor (const QColor & color);

	/**
	 * @brief setNodeWithKeyColor Sets the new color for ConfigNode s that contain a kdb::Key
	 * @param color The new color for ConfigNodes that contain a kdb::Key
	 */
	void setNodeWithKeyColor (const QColor & color);

	/**
	 * @brief setNodeWithoutKeyColor Sets the new color for ConfigNode s that do not contain a kdb::Key
	 * @param color The new color for ConfigNode s that do not contain a kdb::Key
	 */
	void setNodeWithoutKeyColor (const QColor & color);

	/**
	 * @brief set if the system icon theme should be used instead of qt guis own icon set
	 * @param use bool if the system icon theme should be used or not
	 */
	void useSystemIconTheme (const bool & use);

	void setViewermode (bool vmode);

signals:
	/**
	 * @brief highlightColorChanged This signal is emitted if the highlight color is changed. The view will update accordingly.
	 */
	void highlightColorChanged ();

	/**
	 * @brief frameColorChanged This signal is emitted if the frame color is changed. The view will update accordingly.
	 */
	void frameColorChanged ();

	/**
	 * @brief nodeWithKeyColorChanged This signal is emitted if the color for @link ConfigNode s that contain a @link kdb::Key
	 * is changed. The view will update accordingly.
	 */
	void nodeWithKeyColorChanged ();

	/**
	 * @brief nodeWithoutKeyColorChanged This signal is emitted if the color for @link ConfigNode s that do not contain
	 * a @link kdb::Key is changed. The view will update accordingly.
	 */
	void nodeWithoutKeyColorChanged ();

	/**
	 * @brief This signal is emitted if the setting to use system icon theme has changed.
	 */
	void useSystemIconThemeChanged ();

	/**
	 * @brief This signal is emitted if the viewermode setting has changed.
	 */
	void viewermodeChanged ();

private:
	void setDefaults ();
	void getKDB ();

	kdb::KeySet m_config;

	QColor m_highlightColor;
	QColor m_frameColor;
	QColor m_nodeWithKeyColor;
	QColor m_nodeWithoutKeyColor;

	bool m_useSystemIconTheme;
	bool m_viewermode;

	std::string m_profile;

	std::string m_base;
	std::string m_highlightColorString;
	std::string m_frameColorString;
	std::string m_nodeWKeyColorString;
	std::string m_nodeWOKeyColorString;
	std::string m_useSystemIconThemeString;

	std::string m_legacyBase;
	std::string m_legacyHighlightColorString;
	std::string m_legacyFrameColorString;
	std::string m_legacyNodeWKeyColorString;
	std::string m_legacyNodeWOKeyColorString;

	std::string m_viewermodeString;

	/**
	 * @brief appendColor Updates a color. On the next @link setKDB() action this color will be permanent.
	 * @param keyName The type of the color (e.g. "highlight_color")
	 * @param color The color.
	 */
	void appendColor (const std::string & keyName, const QColor & color);

	/**
	 * @brief setBool Updates a boolean setting. On the next @link setKDB() action this settings value will be permanent.
	 * @param keyName The name of the setting (e.g. "use_system_icon_theme")
	 * @param value The boolean value for the option
	 */
	void appendBool (const std::string & keyName, const bool value);

	/**
	 * @brief lookupColor Retrieves the current color for an item.
	 * @param keyName The type of the color (e.g. "highlight_color")
	 * @param color the color to set
	 */
	void lookupColor (const std::string & keyName, QColor & color) const;

	/**
	 * @brief lookup bool option retrieves the current boolean value for an option.
	 * @param keyName The name of the setting (e.g. "use_system_icon_theme")
	 * @param value the value of the looked up setting
	 */
	void lookupBool (const std::string & keyName, bool & value) const;
};

Q_DECLARE_METATYPE (GUISettings)

#endif // GUISETTINGS_HPP
