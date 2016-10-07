#!/bin/bash
for i in $(seq 100 150)
do
   kdb set dir/configs/apache/hive/database/sample$i ""
   kdb set dir/configs/apache/hive/database/sample$i/key1 "value1"
   kdb setmeta dir/configs/apache/hive/database/sample$i "author" "Namoshek"
   kdb setmeta dir/configs/apache/hive/database/sample$i "title" "A sweet configuration snippet"
   kdb setmeta dir/configs/apache/hive/database/sample$i "description" "Random description $i"
   kdb setmeta dir/configs/apache/hive/database/sample$i "createdat" $i
   kdb setmeta dir/configs/apache/hive/database/sample$i "tags" "auto-generated"
   kdb setmeta dir/configs/apache/hive/database/sample$i "plugin" "ini"
done
