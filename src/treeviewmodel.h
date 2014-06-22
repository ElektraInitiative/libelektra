#ifndef TREELISTMODEL_H
#define TREELISTMODEL_H

#include <QObject>
#include <QList>
#include <QDebug>
#include <kdb.hpp>
#include <keyio.hpp>

#include "confignode.h"

using namespace std;
using namespace kdb;

class TreeViewModel : public QObject {
    Q_OBJECT

    Q_PROPERTY(QVariantList model READ getModel() NOTIFY modelChanged())

public:
    explicit TreeViewModel();
    QVariantList getModel();
    void sink(ConfigNode *node, QStringList keys, QString path);
    Q_INVOKABLE void synchronize();

private:
    QList<ConfigNode*> m_model;
    void populateModel();

signals:
    void modelChanged();

public slots:

};

#endif // TREELISTMODEL_H
