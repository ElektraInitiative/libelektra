#include <ckdb.h>
#include <iostream>
#include <string>

using namespace std;

ckdb config ("sw/test");
void testSetComments();
void testGetComments();
void testSetNames();
void testGetNames(string);
void testGetSystem();
void testSetAccess();
void testBool();

int main()
{
	testGetNames("t1");
	testGetSystem();
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
	cout << "[" << config.getCommentSize ("t2") << "] " << config.getComment ("t2") << endl;
	cout << "[" << config.getCommentSize ("t3") << "] " << config.getComment ("t3") << endl;
	cout << "[" << config.getCommentSize ("t4") << "] " << config.getComment ("t4") << endl;
}

void testSetNames()
{
	config.set ("t1", "H1re is the value");
	config.set ("t2", "Also& §#ä work 2");
	config.set ("t3", "What about that HER3");
	config.set ("t4", "Just 4 values!");
}

void testGetNames(std::string str)
{
	cout << "Name:    \t[ " << config.getNameSize(str) << "  ]\t" << config.getName(str) << endl;
	cout << "FullName:\t[ " << config.getFullNameSize(str) << "  ]\t" << config.getFullName(str) << endl;
	cout << "RootName:\t[ " << config.getRootNameSize(str) << "  ]\t" << config.getRootName(str) << endl;
	cout << "FullRootName:\t[ " << config.getFullRootNameSize(str) << "  ]\t" << config.getFullRootName(str) << endl;
	cout << "BaseName:\t[ " << config.getBaseNameSize(str) << "  ]\t" << config.getBaseName(str) << endl;
}

void testGetSystem()
{
	cout << "Username: " << config.getOwner("t1") << endl;
	cout << "UID: " << config.getUID("t1") << endl;
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


