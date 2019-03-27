parser grammar YAML;

options {
  tokenVocab=YAML;
}

yaml : STREAM_START (comment* child | comment*) STREAM_END EOF ;
child : (value | map | sequence) comment* ;

value : scalar ;
scalar : PLAIN_SCALAR
       | SINGLE_QUOTED_SCALAR
       | DOUBLE_QUOTED_SCALAR
       ;

map : MAP_START pairs MAP_END ;
pairs : pair+ ;
pair : KEY key
       VALUE
       comment* // Match possible comment, even if there is no value (child)
       child?
     ;
key : scalar ;

sequence : SEQUENCE_START elements SEQUENCE_END ;
elements : element+ ;
element : ELEMENT comment* child;

comment : COMMENT ;
