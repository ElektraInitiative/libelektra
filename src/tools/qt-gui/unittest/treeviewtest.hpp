/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef TREEVIEWTEST_HPP
#define TREEVIEWTEST_HPP

#include "../src/treeviewmodel.hpp"
#include <QObject>
#include <QTest>

class TreeViewTest : public QObject
{
	Q_OBJECT

private slots:
	void initTestCase ();
	void cleanupTestCase ();

private:
	TreeViewModel * model;
};

#endif // TREEVIEWTEST_HPP
