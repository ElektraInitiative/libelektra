#ifndef CONFIGNODETEST_HPP
#define CONFIGNODETEST_HPP

#include <QTest>

class ConfigNodeTest : public QObject
{
    Q_OBJECT

private slots:

    void getChildCountTest();
    void getNameTest();
    void getPathTest();
    void getValueTest();
    void setNameTest();
    void setValueTest();
    void appendChildTest();
    void hasChildTest();
    void getChildrenTest();
    void getMetaValueTest();
    void ChildrenHaveNoChildrenTest();
    void getChildByNameTest();
    void getChildByIndexTest();
    void setMetaTest();
    void deleteMetaTest();
    void getKeyTest();

};

#endif // CONFIGNODETEST_HPP
