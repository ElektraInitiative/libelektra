- infos = Information about the template plugin is in keys below
- infos/author = e1528532 <e1528532@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/xml
- infos/placements = getstorage setstorage
- infos/status = recommended experimental unittest
- infos/description = Storage in the XML format.
- infos/needs =

## Introduction

This plugin is a storage plugin allowing Elektra to read and write XML
formatted files. It uses a general format which:
- Maps key names to XML elements
- Maps key values to textual content of XML elements
- Maps metakeys to XML attributes. Metakey name = attribute name, Metakey value 
 	= attribute value
- Ignores XML comments

## Usage

To mount an XML file we use:

    kdb mount file.xml user/test/file xerces

The strength and usage of this plugin is that it supports arbitrary XML files and
does not require a specific format. Given the following example of an XML file:

	<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
	<xerces>foo
	  <bar meta="da_ta">bar</bar>
	</xerces>

We can observe the following result after mounting:

	kdb get user/test/file/xerces > foo
	kdb get user/test/file/bar > bar
	kdb getmeta user/test/xerces/bar meta > da_ta

To export an existing keyset to the XML format:

    kdb export user/test/xerces xerces > example.xml

The root element of the resulting XML file will be "xerces".

## Dependencies

- `Xerces-C++ 3.0.0` or newer

## Limitations

This plugin is not able to handle key names which contain characters that are not 
allowed to appear as an XML element name. Consider using the rename plugin to
take care about proper escaping.

The main rules of an XML element name are:
- Element names must start with a letter or underscore
- Element names cannot start with the letters xml (or XML, or Xml, etc)
- Element names can contain letters, digits, hyphens, underscores, and periods
- Element names cannot contain spaces

XSD transformations, schemas or DTDs are not supported yet.
