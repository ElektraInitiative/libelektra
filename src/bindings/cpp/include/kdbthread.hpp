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

/// Subject from Observer pattern for ThreadContext
class ThreadSubject
{
public:
	virtual void notify(KeySet &ks) = 0;
};

/// A vector of layers
typedef std::vector<std::shared_ptr<Layer>> LayerVector;

typedef std::unordered_map<std::string, std::function<void()>> FunctionMap;

/// A data structure that is stored by context inside the Coordinator
struct PerContext
{
	KeySet toUpdate;
	LayerVector toActivate;
	LayerVector toDeactivate;
};

/**
 * @brief Thread safe coordination of ThreadContext per Threads.
 */
class Coordinator
{
public:
	void onLayerActivation(std::string layerid, std::function <void()> f)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnActivate);
		m_onActivate[layerid] = f;
	}

	void onLayerDeactivation(std::string layerid, std::function <void()> f)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnDeactivate);
		m_onDeactivate[layerid] = f;
	}

private:
	friend class ThreadContext;

	void attach(ThreadSubject *c)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		m_updates.insert(std::make_pair(c, PerContext()));
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
		KeySet & toUpdate = m_updates[c].toUpdate;
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
			c.second.toUpdate.append(k);
		}
		return k;
	}

	/**
	 * @brief Request that some layer needs to be globally
	 * activated.
	 *
	 * @param cc requests it and already has it updated itself
	 * @param layer to activate for all threads
	 */
	void globalActivate(ThreadSubject *cc, std::shared_ptr<Layer> layer)
	{
		{
			std::lock_guard<std::mutex> lock (m_mutexOnActivate);
			for (auto && f: m_onActivate)
			{
				f.second();
			}
			m_onActivate.clear();
		}

		std::lock_guard<std::mutex> lock (m_mutex);
		for (auto & c: m_updates)
		{
			 // caller itself has it already activated
			if (cc == c.first) continue;
			c.second.toActivate.push_back(layer);
		}
	}

	void globalDeactivate(ThreadSubject *cc, std::shared_ptr<Layer> layer)
	{
		{
			std::lock_guard<std::mutex> lock (m_mutexOnDeactivate);
			for (auto && f: m_onDeactivate)
			{
				f.second();
			}
			m_onDeactivate.clear();
		}

		std::lock_guard<std::mutex> lock (m_mutex);
		for (auto & c: m_updates)
		{
			 // caller itself has it already deactivated
			if (cc == c.first) continue;
			c.second.toDeactivate.push_back(layer);
		}
	}

	/**
	 * @param cc requester of its updates
	 *
	 * @see globalActivate
	 * @return all layers for that subject
	 */
	LayerVector fetchGlobalActivation(ThreadSubject *cc)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		LayerVector ret;
		ret.swap(m_updates[cc].toActivate);
		return std::move(ret);
	}

	LayerVector fetchGlobalDeactivation(ThreadSubject *cc)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		LayerVector ret;
		ret.swap(m_updates[cc].toDeactivate);
		return std::move(ret);
	}

	/// stores per context updates not yet delievered
	std::unordered_map<ThreadSubject *, PerContext> m_updates;
	/// mutex protecting m_updates
	std::mutex m_mutex;
	FunctionMap m_onActivate;
	std::mutex m_mutexOnActivate;
	FunctionMap m_onDeactivate;
	std::mutex m_mutexOnDeactivate;
};

class ThreadContext : public ThreadSubject, public Context
{
public:
	typedef std::reference_wrapper<ValueSubject> ValueRef ;

	explicit ThreadContext(Coordinator & gc) : m_gc(gc)
	{
		m_gc.attach(this);
	}

	~ThreadContext()
	{
		m_gc.detach(this);
	}

	template <typename T, typename... Args>
	std::shared_ptr<Layer> activate(Args&&... args)
	{
		std::shared_ptr<Layer>layer = Context::activate<T>(std::forward<Args>(args)...);
		m_gc.globalActivate(this, layer);
		return layer;
	}

	template <typename T, typename... Args>
	std::shared_ptr<Layer> deactivate(Args&&... args)
	{
		std::shared_ptr<Layer>layer = Context::deactivate<T>(std::forward<Args>(args)...);
		m_gc.globalDeactivate(this, layer);
		return layer;
	}

	template <typename T>
	void onLayerActivation(std::function <void()> f)
	{
		std::shared_ptr<Layer>layer = std::make_shared<T>();
		m_gc.onLayerActivation(layer->id(), f);
	}

	template <typename T>
	void onLayerDeactivation(std::function <void()> f)
	{
		std::shared_ptr<Layer>layer = std::make_shared<T>();
		m_gc.onLayerDeactivation(layer->id(), f);
	}

	void onLayerActivation(std::string const & layerid, std::function <void()> f)
	{
		m_gc.onLayerActivation(layerid, f);
	}

	void onLayerDeactivation(std::string const & layerid, std::function <void()> f)
	{
		m_gc.onLayerDeactivation(layerid, f);
	}


	void syncActivate() // syncLayers?
	{
		Events e;
		for(auto const & l: m_gc.fetchGlobalActivation(this))
		{
			lazyActivateLayer(l);
			e.push_back(l->id());
		}
		for(auto const & l: m_gc.fetchGlobalDeactivation(this))
		{
			lazyDeactivateLayer(l);
			e.push_back(l->id());
		}
		notifyByEvents(e);
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
