/**
 * @file
 *
 * @brief XDG QQuickImageProvider (original source from http://gitlab.unique-conception.org/qt-libraries/lib-qt-qml-tricks)
 * @author Gabriel Rauter (rauter.gabriel@gmail.com)
 * @author Thomas Boutroue (thebootroo@gmail.com)
 * @copyright relicenced under BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef QQUICKTHEMEICONPROVIDER_H
#define QQUICKTHEMEICONPROVIDER_H

#include <QObject>
#include <QPixmap>
#include <QQuickImageProvider>

class QQuickThemeIconProvider : public QQuickImageProvider
{
public:
	explicit QQuickThemeIconProvider (void);

	QPixmap requestPixmap (const QString & id, QSize * actualSize, const QSize & requestedSize);
};

#endif // QQUICKTHEMEICONPROVIDER_H
