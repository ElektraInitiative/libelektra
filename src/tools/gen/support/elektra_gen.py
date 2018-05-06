import os

from gen import Support


class Enum:
	def __init__(self, prefix, type, type_name, values):
		self.type = type
		self.type_name = type_name
		self.values = values
		self.prefix = prefix

	def __str__(self):
		return "type=" + self.type + ", type_name=" + self.type_name + ", values" + str(self.values)

	def __eq__(self, other):
		return self.type == other.type and self.type_name == other.type_name and self.values == other.values

	@classmethod
	def fromKeyInfo(cls, support, key, info):
		if "gen/enumtype" in info:
			type_name = "Enum" + info["gen/enumtype"]
			prefix = "ELEKTRA_ENUM_" + support.key_to_uppercase(info["gen/enumtype"])
		else:
			normalized = support.normalized_key(key)
			type_name = "Enum" + support.key_to_camelcase(normalized)
			prefix = "ELEKTRA_ENUM_" + support.key_to_uppercase(normalized)

		type = "Elektra" + type_name
		values = support.toarray(info, "check/enum")

		return Enum(prefix, type, type_name, values)


class ElektraGenSupport(Support):
	def __init__(self):
		pass

	def enums(self, parameters):
		enums = {}
		for key, info in parameters.iteritems():
			if self.type_of(info) != "enum":
				continue

			this_enum = Enum.fromKeyInfo(self, key, info)
			if this_enum.type in enums:
				other_enum = enums[this_enum.type]
				if other_enum == this_enum:
					continue
				else:
					raise Exception("different enums with same gen/enumtype: { "
									+ ', '.join(this_enum.values) + " } vs { "
									+ ', '.join(other_enum.values) + " }")

			enums[this_enum.type] = this_enum

		return enums.values()

	def include_guard(self, template):
		filename = os.path.basename(template)
		return "__" + filename.upper().replace('.', '_') + "__"

	def header_file(self, output):
		filename = os.path.basename(output)
		return filename[:-1] + "h"

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
		return key.upper().replace('/', '_').replace('#', '')

	def key_to_camelcase(self, key):
		return ''.join(x for x in key.title() if not x == '/' and not x == '_')

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

		if type in ["enum", "string", "boolean", "char", "octet", "short", "unsigned_short", "long", "unsigned_long", "long_long",
					"unsigned_long_long", "float", "double", "long_double"]:
			return type
		else:
			raise Exception("Invalid type: '" + type + "'")

	def kdb_type_to_tag_type(self, type):
		result = ""
		for part in type.split('_'):
			result += self.upcase_first_letter(part)

		return result

	def tag_type(self, key, info):
		"""Return the type for given parameter"""
		type = self.type_of(info)
		if type == "enum":
			return Enum.fromKeyInfo(self, key, info).type_name
		else:
			return self.kdb_type_to_tag_type(type)

	def check_default(self, key, info):
		if "default" in info:
			return True
		else:
			raise Exception("Key '" + key + "' doesn't have a default value!")

	def default_value(self, key, info):
		value = info["default"]
		type = self.type_of(info)
		if type == "enum":
			enum_values = Enum.fromKeyInfo(self, key, info).values
			return enum_values.index(value)
		else:
			if value.startswith('"') and value.endswith('"'):
				value = value[1:]
				value = value[:-1]
			return value

	def enum_typedef(self, enum):
		result = "typedef enum {\n"

		for i, v in enumerate(enum.values):
			case = "\t" + enum.prefix + "_" + v.upper()
			result += case + " = " + str(i) + ",\n"

		result += "} " + enum.type + ";"

		return result


if __name__ == "__main__":
	import doctest

	doctest.testmod()
