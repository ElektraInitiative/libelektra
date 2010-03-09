int nbStreaming;

void init(void)
{
	nbStreaming =  loadToolsLib();
	warn_if_fail (nbStreaming == 0, "Unable to load elektratools, no stream testing will be done");
}

int loadToolsLib(void)
{
	kdbLibHandle dlhandle=0;

	kdbLibInit();

	dlhandle=kdbLibLoad("libelektratools");
	if (dlhandle == 0) {
		return 1;
	}
	
	ksFromXMLfile=(KSFromXMLfile)kdbLibSym(dlhandle,"ksFromXMLfile");
	ksFromXML=(KSFromXML)kdbLibSym(dlhandle,"ksFromXML");

	ksToStream = (output) kdbLibSym (dlhandle, "ksToStream");
	ksOutput   = (output) kdbLibSym (dlhandle, "ksOutput");
	ksGenerate = (output) kdbLibSym (dlhandle, "ksGenerate");

	return 0;
}

