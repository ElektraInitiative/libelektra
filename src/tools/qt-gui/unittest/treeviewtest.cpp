/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./treeviewtest.hpp"
#include <kdb.hpp>

using namespace kdb;

void TreeViewTest::initTestCase ()
{
	Key userBranch1Key1 ("user:/branch1/Key1");
	Key userBranch1Key2 ("user:/branch1/Key2");
	Key userBranch2Branch1Key ("user:/branch2/branch1/Key");

	userBranch1Key1.setString ("branch1Key1: Value");
	userBranch1Key2.setString ("branch1Key2: Value");
	userBranch2Branch1Key.setString ("branch2Branch1Key1: Value");

	userBranch1Key1.setMeta ("userBranch1MetaKey1", "userBranch1MetaValue1");
	userBranch1Key1.setMeta ("userBranch1MetaKey2", "userBranch1MetaValue2");

	KeySet set;

	set.append (userBranch1Key1);
	set.append (userBranch1Key2);
	set.append (userBranch2Branch1Key);

	model = new TreeViewModel (set);

	//    PrintVisitor printer;
	//    model->accept(printer);
}

void TreeViewTest::cleanupTestCase ()
{
	delete model;
}

QTEST_MAIN (TreeViewTest)
