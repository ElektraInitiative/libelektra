// clang-format off
ksNew (16,
    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),

    keyNew (PREFIX "/table_array_top",
        KEY_META, "array", "#1",
        KEY_META, "type", "tablearray",
        KEY_END
    ),
    
    keyNew (PREFIX "/table_array_top/#0/a", KEY_VALUE, "5", KEY_END),
    keyNew (PREFIX "/table_array_top/#0/sub1/sub2/sub3", KEY_META, "array", "#2", KEY_END),
    
    keyNew (PREFIX "/table_array_top/#0/sub1/sub2/sub3/#0/b", KEY_VALUE, "1", KEY_END),
    keyNew (PREFIX "/table_array_top/#0/sub1/sub2/sub3/#0/c", KEY_VALUE, "2", KEY_END),
   
    keyNew (PREFIX "/table_array_top/#0/sub1/sub2/sub3/#1/b", KEY_VALUE, "666", KEY_END),
    keyNew (PREFIX "/table_array_top/#0/sub1/sub2/sub3/#1/c", KEY_VALUE, "1337", KEY_END),
    
    keyNew (PREFIX "/table_array_top/#0/sub1/sub2/sub3/#2/b", KEY_VALUE, "1234", KEY_END),
    keyNew (PREFIX "/table_array_top/#0/sub1/sub2/sub3/#2/c", KEY_VALUE, "5678", KEY_END),
   

    keyNew (PREFIX "/table_array_top/#1/a", KEY_VALUE, "8", KEY_END),
    keyNew (PREFIX "/table_array_top/#1/sub1/sub2/sub3",
        KEY_META, "array", "#1",
        KEY_META, "type", "tablearray",
        KEY_END
    ),

    keyNew (PREFIX "/table_array_top/#1/sub1/sub2/sub3/#0/b", KEY_VALUE, "3", KEY_END),
    keyNew (PREFIX "/table_array_top/#1/sub1/sub2/sub3/#0/c", KEY_VALUE, "9", KEY_END),

    keyNew (PREFIX "/table_array_top/#1/sub1/sub2/sub3/#1/b", KEY_VALUE, "1111", KEY_END),
    keyNew (PREFIX "/table_array_top/#1/sub1/sub2/sub3/#1/c", KEY_VALUE, "2222", KEY_END),

    KS_END
)
