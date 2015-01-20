#include <kdbthread.hpp>

#include <gtest/gtest.h>

using namespace kdb;

void foo1(Coordinator & gc, KeySet & ks)
{
	Key specKey("/hello", KEY_CASCADING_NAME, KEY_END);

	ThreadContext c1(gc);
	ThreadValue<int> v1(ks, c1, specKey);
	assert(v1 == 8);

	v1 = 5;
	assert(v1 == 5);

	std::this_thread::sleep_for(std::chrono::seconds(1));
	assert(v1 == 5);
}

void foo2(Coordinator & gc, KeySet & ks)
{
	Key specKey("/hello", KEY_CASCADING_NAME,  KEY_END);

	ThreadContext c2(gc);
	ThreadValue<int> v2(ks, c2, specKey);
	assert (v2 == 5);

	std::this_thread::sleep_for(std::chrono::seconds(1));
	c2.update();
	assert (v2 == 5);

	v2 = 12;
	assert (v2 == 12);
}

TEST(test_contextual_thread, instanciation)
{
	Key specKey("/hello", KEY_CASCADING_NAME,  KEY_END);

	KeySet ks;
	ks.append(Key("user/hello", KEY_VALUE, "22", KEY_END));

	Coordinator gc;
	ThreadContext c(gc);
	ThreadValue<int> v(ks, c, specKey);
	assert(v == 22);

	v = 8;
	assert (v== 8);

	std::thread t1(foo1, std::ref(gc), std::ref(ks));

	std::this_thread::sleep_for(std::chrono::milliseconds(500));
	c.update();
	assert (v == 5);

	std::thread t2(foo2, std::ref(gc), std::ref(ks));
	t1.join();
	t2.join();

	c.update();
	assert (v == 12);

	ks.append(Key("user/activate", KEY_VALUE, "88", KEY_END));
	v.activate();
	assert (v==88);
}
