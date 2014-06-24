#include "treeviewmodel.h"
#include "confignode.h"

TreeViewModel::TreeViewModel(QQmlContext *ctxt)
{
    m_ctxt = ctxt;
}

QVariantList TreeViewModel::getModel(){

    populateModel();

    QVariantList model;

    foreach(ConfigNode *node, m_model){
        model.append(QVariant::fromValue(node));
    }

    return model;
}

void TreeViewModel::sink(ConfigNode *node, QStringList keys, QString path){

    if(keys.length() == 0)
        return;

    QString name =  keys.takeFirst();

    if(node->hasChild(name)){
        sink(node->getChildByName(name), keys, node->getPath() + "/" + name);
    }
    else{
        ConfigNode *newNode = new ConfigNode(name,(path + "/" + name));
        node->appendChild(newNode);
        sink(newNode, keys, node->getPath() + "/" + name);
    }
}

void TreeViewModel::synchronize()
{
    m_ctxt->setContextProperty("externTreeModel", QVariant::fromValue(this));
}

void TreeViewModel::deleteKey(QString path)
{
    m_config.cut(m_config.lookup(path.toStdString()));
    m_kdb.set(m_config, "/");
    synchronize();
}

void TreeViewModel::populateModel()
{
    m_config.clear();
    m_kdb.get(m_config, "/");
    m_config.rewind();

    ConfigNode *system = new ConfigNode("system", "system");
    ConfigNode *root = new ConfigNode("user", "user");

    m_model.clear();
    m_model << system << root;

    QStringList configData;

    while(m_config.next()){
        configData << QString::fromStdString(m_config.current().getName());
//        qDebug() << QString::fromStdString(m_config.current().getName());
    }

    for(int i = 0; i < configData.length(); i++){

        QStringList tmpKeys = configData.at(i).split("/");

        if(tmpKeys.at(0) == "system"){
            tmpKeys.removeFirst();
            sink(m_model.at(0), tmpKeys, "system");
        }
        else if(tmpKeys.at(0) == "user"){
            tmpKeys.removeFirst();
            sink(m_model.at(1), tmpKeys, "user");
        }
        else{
            qDebug() << "INVALID_KEY";
        }

    }

    qDebug() << "POPULATED===========================================!";
}
