#include <ckdb.h>
#include <iostream>
#include <string>

using namespace std;

ckdb config ("sw/test");
void testSetComments();
void testGetComments();
void testSetNames();
void testGetNames();
void testGetSystem();
void testSetAccess();
void testBool();

int main()
{
//TODO: Binärdaten werden automatisch gelöscht!
	
	// char * write = "somebinary";
	// config.setBinary ("i", (void *) write, 10);
	
	char * read = new char [10];
	// config.getBinary ("i", (void *) read, 10);
	// cout << read << endl;
	
	/*Key * k = new Key;
	keyInit (k);
	keySetName (k, "user/sw/test/bin");
	
	kdbOpen ();
	kdbSetKey (k);
	kdbClose ();
	
	keyClose (k);
	delete k;*/
	
	Key * k = new Key;
	keyInit (k);
	
	kdbOpen ();
	kdbGetKeyByParent ("user/sw/test", "i", k);
	kdbClose();
	
	keyGetString (k,read,10);
	cout << read << endl;
	
	keyClose (k);
	delete k;
	
	return 0;
}

void testSetComments()
{
	config.setComment ("t1", "Here is a Commen1");
	config.setComment ("t2", "W2at about another?");
	config.setComment ("t3", "HEr3 is one too!");
	config.setComment ("t4", "just 4 comment");
}

void testGetComments()
{
	cout << "[" << config.getCommentSize ("t1") << "] " << config.getComment ("t1") << endl;
}

void testSetNames()
{
	config.set ("t1", "H1re is the value");
	config.set ("t2", "Also& §#ä work 2");
	config.set ("t3", "What about that HER3");
	config.set ("t4", "Just 4 values!");
}

void testGetNames()
{
	cout << "Name:    \t[ " << config.getNameSize("t1") << "  ]\t" << config.getName("t1") << endl;
	cout << "FullName:\t[ " << config.getFullNameSize("t1") << "  ]\t" << config.getFullName("t1") << endl;
	cout << "RootName:\t[ " << config.getRootNameSize("t1") << "  ]\t" << config.getRootName("t1") << endl;
	cout << "FullRootName:\t[ " << config.getFullRootNameSize("t1") << "  ]\t" << config.getFullRootName("t1") << endl;
	cout << "BaseName:\t[ " << config.getBaseNameSize("t1") << "  ]\t" << config.getBaseName("t1") << endl;
}

void testGetSystem()
{
	cout << config.getOwner("t1") << "\tUID: " << config.getUID("t1") << endl;
	cout << "GID: " << config.getGID("t1") << endl;
	cout << "Mode: " << oct << config.getAccess ("t1") << endl;
}

void testSetAccess()
{
	config.setAccess ("t1", 0600);
}

void testBool()
{
	config.setBool ("b", false);
	if (config.getBool ("b")) cout << "true";
}


