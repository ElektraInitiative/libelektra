grammar YAML ;

/* ================== */
/* = Subset Grammar = */
/* ================== */

// The following lines contain a simplistic version of a very basic YAML grammar.

mappings : mapping+ ;
mapping : key ':' value ;
key : WORD ;
value : WORD ;

WORD : [a-zA-Z]+ ;
WS : [ \t\r\n]+ -> skip ;
