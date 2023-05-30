-- This file contains the SQL statements that were used for creating the sample SQLite database "elektraDB.db"

CREATE TABLE elektraKeys (
    keyName TEXT PRIMARY KEY NOT NULL,
    keyValue TEXT DEFAULT NULL
);

CREATE TABLE metaKeys (
    keyName TEXT,
    metaKeyName TEXT NOT NULL,
    metaKeyValue TEXT DEFAULT NULL,
    CONSTRAINT fk_metakeys FOREIGN KEY (keyName) REFERENCES elektraKeys (keyName),
    CONSTRAINT pk_metaKeys PRIMARY KEY (keyName, metaKeyName)
);

INSERT INTO elektraKeys (keyName, keyValue) VALUES ('sqliteapp1/key1', 'sqlite val 1.1');
INSERT INTO elektraKeys (keyName, keyValue) VALUES ('sqliteapp1/key2', 'sqlite val 1.2');
INSERT INTO elektraKeys (keyName, keyValue) VALUES ('sqliteapp2/key1', 'sqlite val 2.1');
INSERT INTO elektraKeys (keyName, keyValue) VALUES ('sqliteapp2/key2', 'sqlite val 2.2');
INSERT INTO elektraKeys (keyName, keyValue) VALUES ('sqliteapp2/key3', 'sqlite val 2.3');
INSERT INTO elektraKeys (keyName, keyValue) VALUES ('sqliteapp3/key1', 'sqlite val 3.1');


INSERT INTO metaKeys (keyName, metaKeyName, metaKeyValue) VALUES ('sqliteapp1/key1', 'metakey 1.1.1', 'metaval 1.1.1');
INSERT INTO metaKeys (keyName, metaKeyName, metaKeyValue) VALUES ('sqliteapp1/key1', 'metakey 1.1.2', 'metaval 1.1.2');
INSERT INTO metaKeys (keyName, metaKeyName, metaKeyValue) VALUES ('sqliteapp1/key1', 'metakey 1.1.3', 'metaval 1.1.3');
INSERT INTO metaKeys (keyName, metaKeyName, metaKeyValue) VALUES ('sqliteapp1/key1', 'metakey 1.1.4', 'metaval 1.1.4');
INSERT INTO metaKeys (keyName, metaKeyName, metaKeyValue) VALUES ('sqliteapp2/key2', 'metakey 2.2.1', 'metaval 2.2.1');
INSERT INTO metaKeys (keyName, metaKeyName, metaKeyValue) VALUES ('sqliteapp2/key2', 'metakey 2.2.2', 'metaval 2.2.2');
INSERT INTO metaKeys (keyName, metaKeyName, metaKeyValue) VALUES ('sqliteapp2/key3', 'metakey 2.3.1', 'metaval 2.3.1');
