grammar Test;

ids : id*;
id : ID;

ID : [a-zA-Z]+;
WS : [ \t\r\n]+ -> skip;
