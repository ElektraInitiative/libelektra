#ifndef ELEKTRA_KDBTHREAD_HPP
#define ELEKTRA_KDBTHREAD_HPP

#include <kdb.hpp>

#include <mutex>
#include <thread>
#include <vector>
#include <cassert>
#include <algorithm>
#include <functional>
#include <unordered_map>

#include <kdbcontext.hpp>

namespace kdb
{

typedef std::function<Key()> Post;

class ThreadSubject
{
public:
	virtual void notify(KeySet &ks) = 0;
};

/**
 * @brief Thread safe coordination of ThreadContext per Threads
 */
class Coordinator
{
public:
	void attach(ThreadSubject *c)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		m_updates.insert(std::make_pair(c, KeySet()));
	}

	void detach(ThreadSubject *c)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		m_updates.erase(c);
	}

	/**
	 * @brief Update the given ThreadContext
	 */
	void update(ThreadSubject *c)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		// if (c.m_flag) return;
		KeySet & toUpdate = m_updates[c];
		// if (toUpdate.size() == 0) return;

		c->notify(toUpdate);
		toUpdate.clear();
		// c.m_flag = false;
	}

	/**
	 * @brief Receive a function to be executed and remember
	 * which keys need a update in the other ThreadContexts.
	 */
	Key post(Post post)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		Key k = post();
		for (auto & c: m_updates)
		{
			// c.m_flag = true;
			c.second.append(k);
		}
		return k;
	}

private:
	std::unordered_map<ThreadSubject *, KeySet> m_updates;
	std::mutex m_mutex;
};

class ValueSubject
{
public:
	virtual void notify() = 0;
};

class ThreadContext : public ThreadSubject
{
public:
	typedef std::reference_wrapper<ValueSubject> ValueRef ;
	ThreadContext(Coordinator & gc) : m_gc(gc)
	{
		m_gc.attach(this);
	}

	~ThreadContext()
	{
		m_gc.detach(this);
	}

	void attach(ValueSubject &v, Post post)
	{
		Key key = m_gc.post(post);
		m_keys.insert(std::make_pair(key, ValueRef(v)));
	}

	void notify(KeySet & ks)
	{
		for(auto const & k: ks)
		{
			auto const& f = m_keys.find(k);
			f->second.get().notify();
		}
	}

	void update()
	{
		m_gc.update(this);
	}

	void post(Post post)
	{
		m_gc.post(post);
	}

	bool m_flag;

private:
	Coordinator & m_gc;
	std::unordered_map<Key, ValueRef> m_keys;
};

}
