import uuid
generated_uuid = str(uuid.uuid4()).replace('-','_').upper()

from os.path import basename, dirname

def includeguard(filename):
	if filename == '-':
		return "ELEKTRA_GEN_" + generated_uuid + "_H"
	else:
		return "ELEKTRA_GEN_" + generated_uuid + "_" + filename.replace('.','_').upper()
