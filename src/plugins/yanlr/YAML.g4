parser grammar YAML;

options {
  tokenVocab=YAML;
}

yaml : STREAM_START child? STREAM_END EOF ;
child : value | map | sequence ;

value : scalar ;
scalar : PLAIN_SCALAR
       | DOUBLE_QUOTED_SCALAR
       ;

map : MAPPING_START pairs BLOCK_END ;
pairs : pair+ ;
pair : KEY key VALUE child ;
key : scalar ;

sequence : SEQUENCE_START elements BLOCK_END ;
elements : element+ ;
element : ELEMENT child ;

