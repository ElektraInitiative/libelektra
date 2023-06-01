/**
 * @file
 *
 * @brief XDG QQuickImageProvider (original source from http://gitlab.unique-conception.org/qt-libraries/lib-qt-qml-tricks)
 * @author Gabriel Rauter (rauter.gabriel@gmail.com)
 * @author Thomas Boutroue (thebootroo@gmail.com)
 * @copyright relicenced under BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./QQuickThemeIconProvider.hpp"

#include <QDebug>
#include <QIcon>

QQuickThemeIconProvider::QQuickThemeIconProvider (void) : QQuickImageProvider (QQuickImageProvider::Pixmap)
{
}

QPixmap QQuickThemeIconProvider::requestPixmap (const QString & id, QSize * actualSize, const QSize & requestedSize)
{
	static const QPixmap EMPTY_PIX = QPixmap ();
	static const QSize DEFAULT_SIZE = QSize (128, 128);
	QPixmap ret = EMPTY_PIX;
	const QString name = id;
	const QIcon icon = QIcon::fromTheme (name, QIcon (":/qml/icons/" + name + ".png"));
	if (!icon.isNull ())
	{
		ret = icon.pixmap (requestedSize.isValid () ? requestedSize : DEFAULT_SIZE);
		if (!ret.isNull ())
		{
			if (actualSize != Q_NULLPTR)
			{
				(*actualSize) = ret.size ();
			}
		}
	}
	else
	{
		qWarning () << "No icon named" << name << "in theme !";
	}
	return ret;
}
