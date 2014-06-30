#include <iostream>
#include <merging/threewaymerge.hpp>
#include "keysetio.hpp"
		
		
int main(void){
	using namespace std;
	using namespace kdb;
	using namespace kdb::tools::merging;

		
	KeySet b;
	KeySet o;
	KeySet t;
			
	Key test1("user/merge/base/test1", KEY_END);
	Key test2("user/merge/base/test2", KEY_END);	
	Key test3("user/merge/base/test3", KEY_END);
	Key test4("user/merge/base/test4", KEY_END);
	Key test5("user/merge/base/test5", KEY_END);	
	Key test6("user/merge/base/test6", KEY_END);	
	Key test7("user/merge/base/test/test7", KEY_END);
	Key test8("user/merge/base/test8", KEY_END);
	Key test9("user/merge/base/test9", KEY_END);
	Key test10("user/merge/base/test10", KEY_END);
		
	test1.setString("Test 1");
	test2.setString("Test 2");
	test3.setString("Test 3");
	test4.setString("Test 4");
	test5.setString("Test 5");
	test6.setString("Test 6");
	test7.setString("Test 7");
	test8.setString("Test 8");
	test9.setString("Test 9");
	test10.setString("Test 10");
		
		
	b.append(Key("user/merge/base"));
	b.append(test1);
	b.append(test3);
	b.append(test4);
	b.append(test5);
	b.append(test7);
	b.append(test8);
	b.append(test10);
		
	Key test11("user/merge/ours/test1", KEY_END);
	Key test12("user/merge/ours/test2", KEY_END);
	Key test13("user/merge/ours/test3", KEY_END);		
	Key test14("user/merge/ours/test4", KEY_END);
	Key test15("user/merge/ours/test5", KEY_END);
	Key test16("user/merge/ours/test6", KEY_END);
	Key test17("user/merge/ours/test/test7", KEY_END);
	Key test18("user/merge/ours/test8", KEY_END);
	Key test19("user/merge/ours/test9", KEY_END);
	Key test20("user/merge/ours/test10", KEY_END);
		
	o.append(Key("user/merge/ours"));
	o.append(test11);
	o.append(test12);		
	o.append(test14);
	o.append(test15);
	o.append(test17);
	o.append(test18);
	o.append(test20);		
	
	Key test21("user/merge/theirs/test1", KEY_END);		
	Key test22("user/merge/theirs/test2", KEY_END);
	Key test23("user/merge/theirs/test3", KEY_END);
	Key test24("user/merge/theirs/test4", KEY_END);		
	Key test25("user/merge/theirs/test5", KEY_END);
	Key test26("user/merge/theirs/test6", KEY_END);
	Key test27("user/merge/theirs/test/test7", KEY_END);
	Key test28("user/merge/theirs/test8", KEY_END);		
	Key test29("user/merge/theirs/test9", KEY_END);		
	Key test30("user/merge/theirs/test10", KEY_END);
		
		
	t.append(Key("user/merge/theirs"));
	t.append(test21);
	t.append(test25);
	t.append(test27);
	t.append(test28);
	t.append(test30);
		
	Key merge_root("user/merge/merged", KEY_END);
		
	cout << "Base:" << endl << b << endl;	
	cout << endl << "Ours:" << endl << o << endl;	
	cout << endl << "Theirs:" << endl << t << endl;
		
	MergeResult m = ThreeWayMerge::mergeKeySet(b, o, t, merge_root);
		
	cout << endl << "Merged:" << endl << m.getMergedKeys() << endl;

	return 0;
		
}
