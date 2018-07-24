parser grammar YAML;

options {
  tokenVocab=YAML;
}

yaml : STREAM_START child? comment* STREAM_END EOF ;
child : comment* (value | map | sequence) comment*;

value : scalar ;
scalar : PLAIN_SCALAR
       | DOUBLE_QUOTED_SCALAR
       ;

map : MAPPING_START pairs BLOCK_END ;
pairs : pair+ ;
pair : KEY key
       VALUE
       comment* // Match possible comment, even if there is no value (child)
       child?
      ;
key : scalar ;

sequence : SEQUENCE_START elements BLOCK_END ;
elements : element+ ;
element : ELEMENT child ;

comment : COMMENT ;
