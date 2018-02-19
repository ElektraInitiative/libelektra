grammar YAML;

mappings : mapping+;
mapping : key ':' value;
key : WORD;
value : WORD;

WORD : [a-zA-Z]+;
WS : [ \t\r\n]+ -> skip;
