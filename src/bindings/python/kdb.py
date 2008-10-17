#!/usr/bin/python

import getopt
import sys
import elektra

class Arguments:
	def __init__(self):
		self.args=0
		self.argAll=None
		self.argFile=None
		self.argComment=None
		self.argDescriptive=None
		self.argLong=None
		self.argDir=None
		self.argFullname=None
		self.argGroup=None
		self.argHelp=None
		self.argShow=True
		self.sargMode=None
		self.argNoSort=True
		self.argNoRecursive=True
		self.argShell=None
		self.sargType=None
		self.argUser=None
		self.argValue=None
		self.argXML=None

commands=('get','set','ls','rm','mv','ln','edit','export','import','monitor','info')

class Help:
	def get(self):
		print """ get
 Get the value from the specified key. Accepts options: -d, -l, -f, -s 

-d
 Causes get to work descriptivelly. When requesting a key it will show the comment, key name and its value in a fancy format 

-l
 Causes to display long results. With ls, will generate lists similar to ls -l. With get, will show also the key name. 

-f
 Causes to work with full key names. A full key name makes sense only on user/* keys, and differentiate from the regular key names in specifying the o wner user. If the current user is someuser, the user/some/key full name is user:someuser/some/key. Makes effect in ls, export and get subcommands.

-s
 Causes get and ls to be more friendly to Shell scripts. For example, when requesting user/env/env2/PATH, the output will be PATH="the value", that is , only the basename of the key will be showed and the value will be surrounded by  ". """
	def set(self):
		print """ set
 Set the value to the specified key. Accepts options: -c, -t, -m, -b 

-c comment
 When setting keys, you can use this argument to set a descriptive comment for it. This comment is exactly as a comment in a plain text configuration file. The comment is stored as UTF-8(7) regardeless of your current encoding ($LANG).
-t type
When setting a key's value, you can specify the type with this switch.
Currently accepted types are:
  string for plain text
  binary for binary as-is values, should be avoided
  link to create symbolic links between keys
  dir to create folder keys, is obsolete now, every key can contain subkeys
Plain text are always stored as UTF-8(7) in Elektra, regardeless of your current encoding ($LANG). Binary values should be avoided, because they are b lack boxes for system administrators.

-m mode
 For the set command. Will set the key mode permission to mode, which must be an octal number as for chmod(1). 

-b filename
 Set the key value as the content of file filename. This option is more usefull when setting binary keys.
"""
	def ls(self):
		print """ls
 As the ls(1) command, list key names for the specified key, or children keys, if specified a folder key. The -v argument will make it show also the v alues of each key. The -d (descriptive) will make it show the comment, key name and its value, as you are watching a plain text file. Accepts options: -x, -d, -l, -f, -v, -R, -s                                                                                                                           
-x
 Makes ls output an XML representation of the keys, instead of an ls-compatible output. 

-d
 Causes get to work descriptivelly. When requesting a key it will show the comment, key name and its value in a fancy format 

-l
 Causes to display long results. With ls, will generate lists similar to ls -l. With get, will show also the key name. 

-f
 Causes to work with full key names. A full key name makes sense only on user/* keys, and differentiate from the regular key names in specifying the o wner user. If the current user is someuser, the user/some/key full name is user:someuser/some/key. Makes effect in ls, export and get subcommands.    
-v
 With the ls subcommand, will make it show also the value stored in the key. 

-R -r
 Causes to work recursively. In ls, will list recursively.
"""
	def rm(self):
		print """rm
As the rm(1) command, removes the key specified
"""
	def mv(self):
		print """mv
Move, or renames a key. Currently it can't move keys across different filesystems.
"""

	def ln(self):
		print """ln
Creates a key that is a symbolic links to another key.
"""

	def edit(self):
		print """edit
A very powerfull subcommand that lets you edit an XML representation of the keys. The parameters it accepts is usually a parent key, so its child keys will be gathered. Can be used with the -R flag to work recursively. The editor used is the one set in the $EDITOR environment variable, or vi. After editing the keys, kdb edit will analyze them and commit only the changed keys, remove the keys removed, and add the keys added.
"""
	def export(self):
		print """export, save 
Export a subtree of keys to XML. If no subtree is defined right after the export command, system and current user trees will be exported. Output is wr itten to standard output. The output encoding will always be UTF-8, regardeless of your system encoding. UTF-8 is the most universal charset you can get when exchanging data between multiple systems. Accepts -f.

-f
 Causes to work with full key names. A full key name makes sense only on user/* keys, and differentiate from the regular key names in specifying the o wner user. If the current user is someuser, the user/some/key full name is user:someuser/some/key. Makes effect in ls, export and get subcommands
"""

	def import_(self):
		print """import, load
Import an XML representation of keys and save it to the keys database. If no filename is passed right after the import command, standard input is used.
"""

	def monitor(self):
		print """monitor, mon, 
Monitor a key for some value change. It will block your command line until a change in the key value is detected, then return its new value.
"""

	def info(self):
		print """info
Displays some information about the Elektra library being used, version, backends, etc.
"""

def usage(retval):
	print """Usage: kdb [OPTION] <command> [<key> [value ...]]
Use kdb to manipulate the Key Database.

ARGUMENTS
Commands are explained with kdb -h command
<key> is the name of the key. It can be prefixed
with environment KDB_ROOT. The slash between will
be inserted.
export KDB_ROOT="user/test/dir"
kdb get file/key ... will expand to user/test/dir/file/key
[value ...] hold the value which should be set

COMMANDS
kdb get [-dlfs] key/name
kdb set [-t type] [-c "A comment about this key"] [-m mode] [-u uid]
	 [-g gid] key/name "the value"
kdb set [-t type] [-m mode] [-c "A comment"] key/name -- "the value"
kdb set [-t type] [-b file] key/name
kdb ls [-lRfvs] [key/dir | key/name]
kdb ls [-lRfvx] [key/dir | key/name] > keys.xml
kdb edit [-R] [key/dir | key/name]
kdb rm key/name
kdb mv key/src key/dest
kdb ln key/src key/dest
kdb export system/some/tree.root > file.xml
kdb import < file.xml
kdb import file.xml
kdb monitor some/key/name
kdb info"""
	sys.exit(retval)

def get():
	if len(arguments.args)!=2: usage(1)
	keyname=arguments.args[1]

	print 'get',keyname

	kdb=elektra.Kdb()
	kdb.open()

	k=elektra.Key()
	k.new(keyname)

	kdb.getKey(k)

	keyvalue=k.getString()
	if arguments.argFullname:
		keyname=k.getFullName()
	if arguments.argShell:
		keyvalue='"'+keyvalue+'"'
	if arguments.argDescriptive:
		print "# Comment: ", k.getComment()
		print "# Type: ", k.getType()
		print "%s=%s"%(keyname,keyvalue)
	elif arguments.argLong:
		print "%s=%s"%(keyname,keyvalue)

	else: print keyvalue

	kdb.close()

def set():
	if len(arguments.args)!=3: usage(1)
	keyname=arguments.args[1]
	value=arguments.args[2]

	print 'set'

	kdb=elektra.Kdb()
	kdb.open()

	k=elektra.Key()
	k.new(keyname)
	k.setName(value)
	if arguments.sargType:
		# TODO put type conversion into elektra.py
		k.setType({'binary':20,'bin':20,'string':40,'dir':0,'link':1}[arguments.sargType])
	if arguments.argComment:
		k.setComment(arguments.argComment)
	k.setString(value)

	kdb.setKey(k)
	kdb.close()

def ls(): # TODO
	if len(arguments.args)!=2: usage(1)
	keyname=arguments.args[1]

	print 'ls'
	kdb=elektra.Kdb()
	kdb.open()

	#k=elektra.Key()
	#k.new("user/key")
	#k.setName("/key")
	#k.setString("hi")

	#kdb.setKey(k)

	ks=kdb.getChildKeys(keyname,norecursive=True)
	#while True:
	#	k=ks.p

	key=ks.lookupByName("/key")
	print key.getString()

	ks.del_()
	kdb.close()

def rm():
	pass

def mv():
	pass

def ln():
	pass

def edit():
	pass

def export():
	pass

def import_():
	pass

def monitor():
	pass

def info():
	pass

def help():
	if len(arguments.args)!=1:
		usage(0)
	h=Help()
	try:
		{'get': h.get, 'set':h.set, 'ls':h.ls, 'rm':h.rm, 'mv':h.mv, 'ln':h.ln, 'edit':h.edit, 
		'export':h.export, 'import':h.import_, 'monitor':h.monitor, 'info':h.info
		}[arguments.args[0]]()
	except:
		usage(0)
	sys.exit(0)

arguments=Arguments()

getopt_args='ab:c:dfg:hilm:nrRst:u:vx'
try:
	optlist,args=getopt.getopt(sys.argv[1:],getopt_args)
except getopt.GetoptError:
	usage(1)

for o,a in optlist:
	#print o,a
	if o=='-a': arguments.argAll=True
	elif o=='-b': arguments.argFile=a
	elif o=='-c': arguments.argComment=a
	elif o=='-d':
		arguments.argDescriptive=True
		arguments.argLong=True
		arguments.argDir=True
	elif o=='-f': arguments.argFullname=True
	elif o=='-g': arguments.argGroup=a
	elif o=='-h': arguments.argHelp=True
	elif o=='-i': arguments.argShow=False
	elif o=='-m': arguments.sargMode=a
	elif o=='-n': arguments.argNoSort=False
	elif o=='-R' or o=='r': arguments.argNoRecursive=False
	elif o=='-s': arguments.argShell=True
	elif o=='-t': arguments.sargType=a
	elif o=='-u': arguments.argUser=a
	elif o=='-v': arguments.argValue=True
	elif o=='-x': arguments.argXML=True
	else:
		usage(1)

arguments.args=args

if arguments.argHelp:
	help()

if len(args)<1: usage(1)
{'get': get, 'set':set, 'ls':ls, 'rm':rm, 'mv':mv, 'ln':ln, 'edit':edit, 
'export':export, 'import':import_, 'monitor':monitor, 'info':info
}[args[0]]()
if not args[0] in commands:
	usage(1)

#print optlist
#print args
