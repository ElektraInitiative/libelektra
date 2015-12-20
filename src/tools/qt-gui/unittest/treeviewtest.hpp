/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef TREEVIEWTEST_HPP
#define TREEVIEWTEST_HPP

#include <QObject>
#include <QTest>
#include "../src/treeviewmodel.hpp"

class TreeViewTest : public QObject
{
    Q_OBJECT

private slots:
    void initTestCase();
    void cleanupTestCase();

private:
    TreeViewModel *model;
};

#endif // TREEVIEWTEST_HPP
