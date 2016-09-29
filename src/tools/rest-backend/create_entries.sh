#!/bin/bash
for i in $(seq 100 300)
do
   kdb set dir/repository/sw/org.apache/hive/database/sample$i ""
   kdb set dir/repository/sw/org.apache/hive/database/sample$i/key1 "value1"
   kdb set dir/repository/sw/org.apache/hive/database/sample$i/key2 "value2"
   kdb set dir/repository/sw/org.apache/hive/database/sample$i/ns1/key1 "valuens1"
   kdb setmeta dir/repository/sw/org.apache/hive/database/sample$i "author" "Namoshek"
   kdb setmeta dir/repository/sw/org.apache/hive/database/sample$i "description" "Random description $i"
   kdb setmeta dir/repository/sw/org.apache/hive/database/sample$i "createdat" $i
   kdb setmeta dir/repository/sw/org.apache/hive/database/sample$i "tags" "auto-generated"
done
