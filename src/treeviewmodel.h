#ifndef TREELISTMODEL_H
#define TREELISTMODEL_H

#include <QObject>
#include <QList>
#include <QDebug>
#include <kdb.hpp>
#include <keyio.hpp>

using namespace std;
using namespace kdb;

class TreeViewModel : public QObject {
    Q_OBJECT

public:
    explicit TreeViewModel();
    QList<QObject*> getModel();
private:
    void populateModel();
    QList<QObject*> model;

signals:

public slots:

};

#endif // TREELISTMODEL_H
