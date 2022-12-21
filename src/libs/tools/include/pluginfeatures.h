"read",				   //< Can read from configuration files
	"write",		   //< Can write to configuration files
	"spec/*/*",		   //< Supports the standard spec for the format, e.g. spec/toml/v1.0.0-rc.3
	"preserves/order",	   //< Preserves order of file structure (see order in doc/METADATA.ini)
	"preserves/comment",	   //< Preserves comments (see comment/# in doc/METADATA.ini)
	"preserves/comment/empty", //< Preserves empty lines in comments
	"preserves/indentation",   //< Preserves indentation around key and values (see indentation in doc/METADATA.ini)
	"limited", //< Only specific structures of KeySets can be stored. This is for special purpose formats like hosts, fstab or passwd.
	"arbitrary/keyset", //< Storage plugins should be able to read any (valid) config file and produce a KeySet. The few plugins marked
			    //with this tag, find also a config file for any KeySet.
	"arbitrary/metadata", //< Also supports any other metadata (not listed in infos/metadata)
	"directory/value",    //< Supports non-leaf keys with value
	"directory/holes",    //< Support for holes within the hierarchy (not every directory needs to be present)
	"type",		      //< Support for Elektra's types
	"array",	      //< Support for arrays
	"format/array",	      //< Has a specific serialization format for arrays (i.e. not #0=value)
	"format/hierarchy",   //< Has a specific serialization format for hierarchy (i.e. not dir/key=value)
	"format/type",	      //< Has a specific serialization format for various types (i.e. not bool="0")
	"format/human/read",  //< The format is intended to be read by humans.
	"format/human/write", //< The format is intended to be written by humans.
