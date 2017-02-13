
#include <ruby.h>
#include <stdio.h>
#define SWIG_TYPE_TABLE kdb
#include "runtime.h"

//#include <kdb.h>

#include <key.hpp>

int mymain(int argc, char* argv[]) {

	ruby_setup();
	if (ruby_setup()) {
		printf("could not init ruby-VM\n");
		exit(1);
	}

	rb_eval_string(""
"require 'kdb'\n"
"class Test\n"
"	def hello\n"
"		puts \"hello world\"\n"
"	end\n"
"	\n"
"	def get_key\n"
"		Kdb::Key.new \"user/test\", value: \"hello key\"\n"
"	end\n"
"end\n"
"");

	rb_eval_string("x = Test.new \n"
			"x.hello");

	// ruby_finalize();

	return 0;
}



VALUE test_m_do_something(VALUE self) {
	// return rb_eval_string("puts \"printing something\"");
	VALUE key = rb_funcall(self, rb_intern("get_key"), 0);
	kdb::Key * cppkey = NULL;
	ckdb::Key * ckey = NULL;
	
	swig_type_info * ti = SWIG_TypeQuery("Kdb::Key");

	int res = SWIG_ConvertPtr(key, (kdb::Key**)(&cppkey), ti, NULL);
	if (res != -1) {
		ckey = cppkey->getKey();
		keySetString(ckey, "my C value");
	}

	return key;
}

extern "C" {
void Init_mytest() {

	mymain(0, NULL);

	VALUE cTest = rb_define_class("Test", rb_cObject);
	rb_define_method(cTest, "do_something", test_m_do_something, 0);

}


int main(int argc, char* argv[]) {
	Init_mytest();
	rb_eval_string("(Test.new).do_something");
	return 0;
}
}
