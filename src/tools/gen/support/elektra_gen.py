from support.gen import *
from support.util import *


class ElektraGenSupport(Support):
	def normalized_key(self, key):
		result = key
		if key.startswith('/'):
			result = result[1:]
		elif key.startswith('user/'):
			result = result[5:]
		elif key.startswith('system/'):
			result = result[7:]
		else:
			raise Exception("invalid keyname '" + key + "'")

		if key.endswith('/'):
			result = result[:-1]
		elif key.endswith('/#'):
			result = result[:-2]

		return result

	def key_to_uppercase(self, key):
		return key.upper().replace('/','_').replace('#','')

	def key_to_camelcase(self, key):
		return ''.join(x for x in key.title() if not x == '/' and not x == '_')

	def tag(self, key):
		return "ELEKTRA_TAG_" + self.tag_name(key)

	def tag_name(self, key):
		normalized = self.normalized_key(key)
		return self.key_to_uppercase(normalized)

	def upcase_first_letter(self, s):
		return s[:1].upper() + s[1:]

	def type_of(self, info):
		if "gen/type" in info:
			type = info["gen/type"]
		elif "type" in info:
			type = info["type"]
		else:
			raise Exception("Missing type.")

		if type in ["enum", "string", "boolean", "char", "octet", "short", "unsigned_short", "long", "unsigned_long", "long_long", "unsigned_long_long", "float", "double", "long_double"]:
			return type
		else:
			raise Exception("Invalid type: '" + type + "'")

	def kdb_type_to_tag_type(self, type):
		result = ""
		for part in type.split('_'):
			result += self.upcase_first_letter(part)

		return ("Elektra" + result + "Tag")

	def tag_type(self, key, info):
		"""Return the type for given parameter"""
		type = self.type_of(info)
		if type == "enum":
			return self.enum_type(key) + "Tag"
		else:
			return self.kdb_type_to_tag_type(type)

	def default_value(self, key, info):
		value = info["default"]
		type = self.type_of(info)
		if type == "enum":
			enum_values = self.toarray(info, "check/enum")
			return enum_values.index(value)
		else:
			if value.startswith('"') and value.endswith('"'):
				value = value[1:]
				value = value[:-1]
			return value

	def enum_typedef(self, key, info):
		normalized = self.normalized_key(key)
		prefix = "ELEKTRA_ENUM_"+ self.key_to_uppercase(normalized)

		result = "typedef enum {\n"

		enum_values = self.toarray(info, "check/enum")
		for i, v in enumerate(enum_values):
			case = prefix + "_" + v.upper()
			result += case + " = " + str(i) + ",\n"

		result += "} " + self.enum_type(key) + ";"

		return result

	def enum_type(self, key):
		return "Elektra" + self.enum_type_name(key)

	def enum_type_name(self, key):
		normalized = self.normalized_key(key)
		return "Enum" + self.key_to_camelcase(normalized)

if __name__ == "__main__":
	import doctest
	doctest.testmod()
