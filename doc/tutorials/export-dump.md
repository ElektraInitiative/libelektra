	$ kdb set system/export_test/a b
	create a new key system/export_test/a with string b

	$ kdb export system/export_test > e.ecf

	$ cat e.ecf
	kdbOpen 1
	ksNew 1
	keyNew 21 2
	system/export_test/ab
	keyEnd
	ksEnd

	$ kdb rm system/export_test/a

	$ kdb get system/export_test/a
	Did not find key

	$ cat e.ecf | kdb import system/export_test

	$ kdb get system/export_test/a
	b

	$ cat e.ecf | kdb import system/export_test_renamed

	$ kdb get system/export_test_renamed/a
	Did not find key

	$ kdb get system/export_test/a
	b
