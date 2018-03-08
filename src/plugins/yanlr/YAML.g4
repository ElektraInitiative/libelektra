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

/* ======== */
/* = YAML = */
/* ======== */

// The following code contain grammar rules from the official YAML specification [YAML]. The rules were modified slightly for ANTLR 4.
//
// [YAML]: http://yaml.org/spec/1.2/spec.html

/* -- Lexer Rules ----------------------------------------------------------------------------------------------------------------------- */

/* ===================== */
/* = 5.1 Character Set = */
/* ===================== */

fragment TAB : '\t' ;
fragment LF : '\n' ;
fragment CR : '\r' ;

// [1] Files can only contain the following set of printable characters.
fragment C_PRINTABLE : TAB | LF | CR | [\u0020-\u007E]
                     | '\u0085' | [\u00A0-\uD7FF] | [\uE000-\uFFFD]
                     | [\u{10000}-\u{10FFFF}]
                     ;

// [2] Quoted YAML scalars can contain almost all characters, except most of the characters from the C0 control block. This rule ensures
//     compatibility with JSON.
fragment NB_JSON : TAB | [\u0020-\u{10FFFF}] ;

/* =========================== */
/* = 5.2 Character Encodings = */
/* =========================== */

// [3]
fragment C_BYTE_ORDER_MARK : '\uFEFF' ;

/* ============================ */
/* = 5.3 Indicator Characters = */
/* ============================ */

// [4]
fragment C_SEQUENCE_ENTRY : '-' ;
// [5]
fragment C_MAPPING_KEY : '?' ;
// [6]
fragment C_MAPPING_VALUE : ':' ;

// [7]
fragment C_COLLECT_ENTRY : ',' ;
// [8]
fragment C_SEQUENCE_START : '[' ;
// [9]
fragment C_SEQUENCE_END : ']' ;
// [10]
fragment C_MAPPING_START : '{' ;
// [11]
fragment C_MAPPING_END : '}' ;

// [12]
fragment C_COMMENT : '#' ;

// [13]
fragment C_ANCHOR : '&' ;
// [14]
fragment C_ALIAS : '*' ;
// [15]
fragment C_TAG : '!' ;

// [16]
fragment C_LITERAL : '|' ;
// [17]
fragment C_FOLDED : '>' ;

// [18]
fragment C_SINGLE_QUOTE : '\'' ;
// [19]
fragment C_DOUBLE_QUOTE : '"' ;

// [20]
fragment C_DIRECTIVE : '%' ;

// [21]
fragment C_RESERVED : '@' | '`' ;

// [22]
fragment C_INDICATOR : C_SEQUENCE_ENTRY
                     | C_MAPPING_KEY
                     | C_MAPPING_VALUE
                     | C_COLLECT_ENTRY
                     | C_SEQUENCE_START
                     | C_SEQUENCE_END
                     | C_MAPPING_START
                     | C_MAPPING_END
                     | C_COMMENT
                     | C_ANCHOR
                     | C_ALIAS
                     | C_TAG
                     | C_LITERAL
                     | C_FOLDED
                     | C_SINGLE_QUOTE
                     | C_DOUBLE_QUOTE
                     | C_DIRECTIVE
                     | C_RESERVED
                     ;

// [23]
fragment C_FLOW_INDICATOR : C_COLLECT_ENTRY
                          | C_SEQUENCE_START
                          | C_SEQUENCE_END
                          | C_MAPPING_START
                          | C_MAPPING_END
                          ;

/* ============================= */
/* = 5.4 Line Break Characters = */
/* ============================= */

// [24]
fragment B_LINE_FEED : '\n' ;
// [25]
fragment B_CARRIAGE_RETURN : '\r' ;
// [26]
fragment B_CHAR : B_LINE_FEED | B_CARRIAGE_RETURN ;

// [27] C_PRINTABLE - B_CHAR - C_BYTE_ORDER_MARK
fragment NB_CHAR : TAB | [\u0020-\u007E]
                 | '\u0085' | [\u00A0-\uD7FF] | [\uE000-\uFEFE] | [\uFF00\uFFFD]
                 | [\u{10000}-\u{10FFFF}]
                 ;

// [28]
fragment B_BREAK : B_CARRIAGE_RETURN? B_LINE_FEED ;
// [29]
fragment B_AS_LINE_FEED : B_BREAK ;
// [30]
fragment B_NON_CONTENT : B_BREAK ;

/* ============================== */
/* = 5.5 White Space Characters = */
/* ============================== */

// [31]
fragment S_SPACE : ' ' ;
// [32]
fragment S_TAB : TAB ;
// [33]
fragment S_WHITE : S_SPACE | S_TAB  ;

// [34] NB_CHAR - S_WHITE
fragment NS_CHAR : [\u0021-\u007E]
                 | '\u0085' | [\u00A0-\uD7FF] | [\uE000-\uFEFE] | [\uFF00\uFFFD]
                 | [\u{10000}-\u{10FFFF}]
                 ;

/* ============================ */
/* = Miscellaneous Characters = */
/* ============================ */

// [35]
fragment NS_DEC_DIGIT : [0-9] ;
// [36]
fragment NS_HEX_DIGIT : NS_DEC_DIGIT | [A-F] | [a-f] ;
// [37]
fragment NS_ASCII_LETTER : [A-Z] | [a-z] ;
// [38]
fragment NS_WORD_CHAR : NS_DEC_DIGIT | NS_ASCII_LETTER | '-' ;
// [39]
fragment NS_URI_CHAR : '%' NS_HEX_DIGIT NS_HEX_DIGIT | NS_WORD_CHAR | '#'
                     | ';' | '/' | '?' | ':' | '@'  | '&' | '=' | '+' | '$' | ','
                     | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' | '[' | ']'
                     ;

// [40] NS_URI_CHAR - '!' - C_FLOW_INDICATOR
fragment NS_TAG_CHAR : '%' NS_HEX_DIGIT NS_HEX_DIGIT | NS_WORD_CHAR | '#'
                     | ';' | '/' | '?' | ':' | '@'  | '&' | '=' | '+' | '$'
                     | '_' | '.'  | '~' | '*' | '\'' | '(' | ')'
                     ;

/* ====================== */
/* = Escaped Characters = */
/* ====================== */

// [41]
fragment C_ESCAPE : '\\' ;
// [42]
fragment NS_ESC_NULL : '0' ;
// [43]
fragment NS_ESC_BELL : 'a' ;
// [44]
fragment NS_ESC_BACKSPACE : 'b' ;
// [45]
fragment NS_ESC_HORIZONTAL_TAB : 't' | TAB ;
// [46]
fragment NS_ESC_LINE_FEED : 'n' ;
// [47]
fragment NS_ESC_VERTICAL_TAB : 'v' ;
// [48]
fragment NS_ESC_FORM_FEED : 'f' ;
// [49]
fragment NS_ESC_CARRIAGE_RETURN : 'r' ;
// [50]
fragment NS_ESC_ESCAPE : 'e' ;
// [51]
fragment NS_ESC_SPACE : ' ' ;
// [52]
fragment NS_ESC_DOUBLE_QUOTE : '"' ;
// [53]
fragment NS_ESC_SLASH : '/' ;
// [54]
fragment NS_ESC_BACKSLASH : '\\' ;
// [55]
fragment NS_ESC_NEXT_LINE : 'N' ;
// [56]
fragment NS_ESC_NON_BREAKING_SPACE : '_' ;
// [57]
fragment NS_ESC_LINE_SEPARATOR : 'L' ;
// [58]
fragment NS_ESC_PARAGRAPH_SEPARATOR : 'P' ;

fragment DOUBLE_HEX : NS_HEX_DIGIT NS_HEX_DIGIT;
fragment QUAD_HEX : DOUBLE_HEX DOUBLE_HEX;
fragment OCTO_HEX : QUAD_HEX QUAD_HEX;

// [59]
fragment NS_ESC_8_BIT : 'x' DOUBLE_HEX ;
// [60]
fragment NS_ESC_16_BIT : 'u' QUAD_HEX ;
// [61]
fragment NS_ESC_32_BIT : 'U' OCTO_HEX ;
// [62]
fragment C_NS_ESC_CHAR : 	C_ESCAPE
                       ( NS_ESC_NULL | NS_ESC_BELL | NS_ESC_BACKSPACE
                       | NS_ESC_HORIZONTAL_TAB | NS_ESC_LINE_FEED
                       | NS_ESC_VERTICAL_TAB | NS_ESC_FORM_FEED
                       | NS_ESC_CARRIAGE_RETURN | NS_ESC_ESCAPE | NS_ESC_SPACE
                       | NS_ESC_DOUBLE_QUOTE | NS_ESC_SLASH | NS_ESC_BACKSLASH
                       | NS_ESC_NEXT_LINE | NS_ESC_NON_BREAKING_SPACE
                       | NS_ESC_LINE_SEPARATOR | NS_ESC_PARAGRAPH_SEPARATOR
                       | NS_ESC_8_BIT | NS_ESC_16_BIT | NS_ESC_32_BIT ) ;
