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
	virtual void syncLayers() = 0;
};

/// A vector of layers
typedef std::vector<std::shared_ptr<Layer>> LayerVector;

typedef std::unordered_map<std::string, std::vector<std::function<void()>>> FunctionMap;

/// A data structure that is stored by context inside the Coordinator
struct PerContext
{
	KeySet toUpdate;
	LayerVector toActivate;
	LayerVector toDeactivate;
};

class ThreadNoContext
{
public:
	/**
	 * @brief attach a new value
	 *
	 * NoContext will never update anything
	 */
	void attachByName(ELEKTRA_UNUSED std::string const & key_name,ELEKTRA_UNUSED  ValueObserver & ValueObserver)
	{}

	/**
	 * @brief The evaluated equals the non-evaluated name!
	 *
	 * @return NoContext always returns the same string
	 */
	std::string evaluate(std::string const & key_name) const
	{
		return key_name;
	}

	/**
	 * @brief (Re)attaches a ValueSubject to a thread or simply
	 *        execute code in a locked section.
	 *
	 * NoContext just executes the function but does so in a
	 * thread-safe way
	 *
	 * @param c the command to apply
	 */
	void execute(Command & c)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		c();
	}
private:
	std::mutex m_mutex;
};

/**
 * @brief Thread safe coordination of ThreadContext per Threads.
 */
class Coordinator
{
public:
	template <typename T>
	void onLayerActivation(std::function <void()> f)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnActivate);
		std::shared_ptr<Layer>layer = std::make_shared<T>();
		m_onActivate[layer->id()].push_back(f);
	}

	template <typename T>
	void onLayerDeactivation(std::function <void()> f)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnDeactivate);
		std::shared_ptr<Layer>layer = std::make_shared<T>();
		m_onDeactivate[layer->id()].push_back(f);
	}

	void onLayerActivation(std::string layerid, std::function <void()> f)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnActivate);
		m_onActivate[layerid].push_back(f);
	}

	void onLayerDeactivation(std::string layerid, std::function <void()> f)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnDeactivate);
		m_onDeactivate[layerid].push_back(f);
	}

	std::unique_lock<std::mutex> requireLock()
	{
		std::unique_lock<std::mutex> lock(m_mutex);
		return std::move(lock);
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
	 * @brief Update the given ThreadContext with newly assigned
	 * values.
	 */
	void updateNewlyAssignedValues(ThreadSubject *c)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		KeySet & toUpdate = m_updates[c].toUpdate;
		if (toUpdate.size() == 0) return;

		c->notify(toUpdate);
		toUpdate.clear();
	}

	/**
	 * @brief Receive a function to be executed and remember
	 * which keys need a update in the other ThreadContexts.
	 */
	void execute(Command & c)
	{
		std::lock_guard<std::mutex> lock (m_mutex);
		c.newKey = c();
		// potentially an assignment took place, notify others
		for (auto & i: m_updates)
		{
			i.second.toUpdate.append(c.newKey);
		}
	}

	void runOnActivate(std::shared_ptr<Layer> layer)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnActivate);
		for (auto && f: m_onActivate[layer->id()])
		{
			f();
		}
		m_onActivate.clear();
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
		runOnActivate(layer);
		cc->syncLayers();

		std::lock_guard<std::mutex> lock (m_mutex);
		for (auto & c: m_updates)
		{
			 // caller itself has it already activated
			if (cc == c.first) continue;
			c.second.toActivate.push_back(layer);
		}
	}

	void runOnDeactivate(std::shared_ptr<Layer> layer)
	{
		std::lock_guard<std::mutex> lock (m_mutexOnDeactivate);
		for (auto && f: m_onDeactivate[layer->id()])
		{
			f();
		}
		m_onDeactivate.clear();
	}


	void globalDeactivate(ThreadSubject *cc, std::shared_ptr<Layer> layer)
	{
		runOnDeactivate(layer);
		cc->syncLayers();

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

	Coordinator & global()
	{
		return m_gc;
	}

	Coordinator & g()
	{
		return m_gc;
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

	void syncLayers()
	{
		// now activate/deactive layers
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

		// pull in assignments from other threads
		m_gc.updateNewlyAssignedValues(this);

	}

	/**
	 * @brief Command dispatching
	 *
	 * @param c the command to execute
	 */
	void execute(Command & c)
	{
		m_gc.execute(c);
		if (c.oldKey)
		{
			m_keys.erase(c.oldKey);
		}
		if (c.newKey)
		{
			m_keys.insert(std::make_pair(c.newKey, ValueRef(c.v)));
		}
	}

	void notify(KeySet & ks)
	{
		for(auto const & k: ks)
		{
			auto const& f = m_keys.find(k);
			if (f == m_keys.end()) continue; // key already had context change
			f->second.get().notifyInThread();
		}
	}

private:
	Coordinator & m_gc;
	/**
	 * @brief A map of values this ThreadContext is responsible for.
	 */
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
