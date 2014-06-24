#ifndef TREELISTMODEL_H
#define TREELISTMODEL_H

#include <QObject>
#include <QList>
#include <QDebug>
#include <QtQml>
#include <kdb.hpp>
#include <keyio.hpp>

#include "confignode.h"

using namespace std;
using namespace kdb;

class TreeViewModel : public QObject {
    Q_OBJECT

    Q_PROPERTY(QVariantList model READ getModel() NOTIFY modelChanged())

public:
    explicit TreeViewModel(QQmlContext*);
    QVariantList getModel();
    void sink(ConfigNode *node, QStringList keys, QString path);

    Q_INVOKABLE void synchronize();
    Q_INVOKABLE void deleteKey(QString path);

private:
    QList<ConfigNode*> m_model;
    void populateModel();
    QQmlContext *m_ctxt;

    KeySet m_config;
    KDB m_kdb;

signals:
    void modelChanged();

public slots:

};

Q_DECLARE_METATYPE(TreeViewModel*)

#endif // TREELISTMODEL_H
