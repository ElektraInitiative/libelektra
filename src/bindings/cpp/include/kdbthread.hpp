#ifndef ELEKTRA_KDBTHREAD_HPP
#define ELEKTRA_KDBTHREAD_HPP

#include <kdbcontext.hpp>

#include <kdb.hpp>

#include <mutex>
#include <thread>
#include <vector>
#include <cassert>
#include <algorithm>
#include <functional>
#include <unordered_map>

namespace kdb
{

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
		KeySet & toUpdate = m_updates[c];
		// if (toUpdate.size() == 0) return;

		c->notify(toUpdate);
		toUpdate.clear();
	}

	/**
	 * @brief Receive a function to be executed and remember
	 * which keys need a update in the other ThreadContexts.
	 */
	Key post(Post postFkt)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		Key k = postFkt();
		for (auto & c: m_updates)
		{
			c.second.append(k);
		}
		return k;
	}

private:
	std::unordered_map<ThreadSubject *, KeySet> m_updates;
	std::mutex m_mutex;
};

class ThreadContext : public ThreadSubject, public Context
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

	void attachToThread(ValueSubject &v, Post postFkt)
	{
		Key key = m_gc.post(postFkt);
		m_keys.insert(std::make_pair(key, ValueRef(v)));
	}

	void notify(KeySet & ks)
	{
		for(auto const & k: ks)
		{
			auto const& f = m_keys.find(k);
			f->second.get().notifyInThread();
		}
	}

	void update()
	{
		m_gc.update(this);
	}

	void post(Post postFkt)
	{
		m_gc.post(postFkt);
	}

	bool m_flag;

private:
	Coordinator & m_gc;
	std::unordered_map<Key, ValueRef> m_keys;
};

template<typename T,
	typename PolicySetter1 = DefaultPolicyArgs,
	typename PolicySetter2 = DefaultPolicyArgs,
	typename PolicySetter3 = DefaultPolicyArgs,
	typename PolicySetter4 = DefaultPolicyArgs,
	typename PolicySetter5 = DefaultPolicyArgs
	>
using ThreadValue = Value <T,
	ContextPolicyIs<ThreadContext>,
	PolicySetter1,
	PolicySetter2,
	PolicySetter3,
	PolicySetter4,
	PolicySetter5
	>;

typedef ThreadValue<uint32_t>ThreadInteger;
typedef ThreadValue<bool>ThreadBoolean;
typedef ThreadValue<std::string>ThreadString;


}

#endif
