#ifndef ELEKTRA_CONTEXTUAL_HPP
#define ELEKTRA_CONTEXTUAL_HPP

#include <set>
#include <map>
#include <vector>
#include <memory>
#include <cassert>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <algorithm>
#include <functional>
#include <unordered_map>

#include <keyset.hpp>

#define ELEKTRA_INLINE __attribute__((noinline))

namespace kdb
{


class Layer
{
public:
	virtual std::string id() const = 0;
	virtual std::string operator()() const = 0;
};



class Observer
{
public:
	virtual ~Observer() = 0;
	virtual void update() const = 0;

	typedef std::reference_wrapper<Observer> reference;
};

bool operator <(Observer const & lhs, Observer const & rhs)
{
	return &lhs < &rhs;
}

inline Observer::~Observer()
{}

class Subject
{
public:
	virtual ~Subject() = 0;
	typedef std::vector<std::string> Events;
	virtual void attach(std::string const & event, Observer &);
	// notify all given events
	virtual void notify(Events const & events) const;
	// notify all events
	virtual void notify() const;

protected:
	Subject();

private:
	typedef std::set<Observer::reference> ObserverSet;
	mutable std::unordered_map<
		std::string,
		ObserverSet> m_observers;
};

inline Subject::Subject()
{}

inline Subject::~Subject()
{}

inline void Subject::attach(std::string const & event, Observer & observer)
{
	auto it = m_observers.find(event);
	if (it == m_observers.end())
	{
		// add first observer for that event
		// (creating a new vector)
		ObserverSet & os = m_observers[event];
		os.insert(std::ref(observer));
	}
	else
	{
		it->second.insert(std::ref(observer));
	}
}

inline void Subject::notify(Events const & events) const
{
	ObserverSet os;
	for (auto & e: events)
	{
		auto it = m_observers.find(e);
		if (it != m_observers.end())
		{
			for (auto & o: it->second)
			{
				os.insert(o); // (discarding duplicates)
			}
		}
#if DEBUG && VERBOSE
		else
		{
			std::cout << "Trying to notify " << e << " but event does not exist" << std::endl;
		}
#endif
	}
	// now call any observer exactly once
	for (auto & o: os)
	{
		o.get().update();
	}
}

inline void Subject::notify() const
{
		Events events;
		for (auto & o: m_observers)
		{
			events.push_back(o.first);
		}
		notify(events);
}



/**
 * @brief Provides a context for configuration
 *
 * Is a subject for observers.
 *
 * Holds currently active layers and allows
 * global/scoped activation of layers.
 */
class Context : public Subject
{
public:
	Context() :
		m_active_layers()
	{
	}

	/**
	 * Lookup value for a current active layer
	 *
	 * @param layer the name of the requested layer
	 * @return the layer
	 */
	std::string operator[](std::string const & layer) const
	{
		auto f = m_active_layers.find(layer);
		if (f != m_active_layers.end())
		{
			assert(f->second && "no null pointers in active_layers");
			return (*f->second)();
		}
		return ""; // this line is surprisingly expensive
	}

	/**
	 * Attach observer using to all events given by
	 * its specification (name)
	 *
	 * @param key_name the name with placeholders to be used for attaching
	 * @param observer the observer to attach to
	 */
	void attachByName(std::string const & key_name, Observer & observer)
	{
		evaluate(key_name, [&](std::string const & current_id, std::string &, bool){
			this->attach(current_id, observer);
			return false;
		});
	}

	/**
	 * Evaluate a specification (name) and return
	 * a key name under current context
	 *
	 * @param key_name the name with placeholders to be evaluated
	 */
	std::string evaluate(std::string const & key_name) const
	{
		return evaluate(key_name, [&](std::string const & current_id, std::string & ret, bool in_group){
			auto f = m_active_layers.find(current_id);
			bool left_group = true;
			if (f != m_active_layers.end())
			{
				assert(f->second && "no null pointers in active_layers");
				std::string r = (*f->second)();
				if (!r.empty())
				{
					if (in_group)
					{
						ret += "%";
					}
					ret += r;
					left_group = false;
				}
				else if (!in_group)
				{
					ret += "%";
				}
			}
			else if (!in_group)
			{
				ret += "%";
			}
			return left_group;
		});
	}

	/**
	 * Evaluate specification with this context.
	 *
	 * @param key_name the keyname with placeholders to evaluate
	 * @param on_layer the function to be called for every
	 *                 placeholder found
	 *
	 * @par on_layer is called for every layer in the
	 * specification.
	 * @return the evaluated string
	*/
	std::string evaluate(std::string const & key_name, std::function<bool(std::string const &, std::string &, bool in_group)> const & on_layer) const
	{
		size_t const & s = key_name.size();
		std::string ret;
		std::string current_id;
		bool capture_id = false; // we are currently within a % block (group or single layer)
		bool left_group = false; // needed to omit layers that do not matter in a group anymore
		bool is_in_group = false;

		// heuristic how much too allocate
		ret.reserve(s*2);
		current_id.reserve(32);

		for (std::string::size_type i=0; i<s; ++i)
		{
			if (key_name[i] == '%')
			{
				if (capture_id)
				{
					// finish capturing
					if (!left_group)
					{
						on_layer(current_id, ret, is_in_group);
					}
					current_id.clear();
					capture_id = false;
				}
				else
				{
					// start capturing
					capture_id = true;
					left_group = false;
					is_in_group = false;
				}
			}
			else if (capture_id && key_name[i] == ' ' && !left_group)
			{
				// found group separator in active
				// group
				left_group = on_layer(current_id, ret, true);
				if (!is_in_group && left_group)
				{
					ret += "%"; // empty groups
				}
				else
				{
					is_in_group = true;
				}
				current_id.clear();
			}
			else // non % character
			{
				if (capture_id)
				{
					current_id += key_name[i];
				}
				else
				{
					ret += key_name[i];
				}
			}
		}

		assert (!capture_id && "number of % incorrect");

		return ret;
	}

private:
	// activates layer, records it, but does not notify
	template <typename T, typename... Args>
	void lazyActivate(Args&&... args)
	{
		std::shared_ptr<Layer>layer = std::make_shared<T>(std::forward<Args>(args)...);
		lazyActivateLayer(layer);
	}

	void lazyActivateLayer(std::shared_ptr<Layer>&layer)
	{
		std::string const & id = layer->id(); // optimisation
		auto p = m_active_layers.emplace(std::make_pair(id, layer));
		if (!p.second)
		{
			m_with_stack.push_back(*p.first);
			p.first->second = layer; // update
		}
		else
		{
			// no layer was not active before, remember that
			m_with_stack.push_back(std::make_pair(id, std::shared_ptr<Layer>()));
		}
#if DEBUG && VERBOSE
		std::cout << "lazy activate layer: " << id << std::endl;
#endif
	}

public:
	/**
	 * @brief Globally activate the layer
	 *
	 * @tparam T the layer to activate
	 * @tparam Args the types for the  arguments to pass to layer construction
	 * @param args the arguments to pass to layer construction
	 */
	template <typename T, typename... Args>
	void activate(Args&&... args)
	{
		std::shared_ptr<Layer>layer = std::make_shared<T>(std::forward<Args>(args)...);
		auto p = m_active_layers.emplace(std::make_pair(layer->id(), layer));
		if (!p.second)
		{
			p.first->second = layer; // update
		}
		notify({layer->id()});
#if DEBUG && VERBOSE
		std::cout << "activate layer: " << layer->id() << std::endl;
#endif
	}

private:
	template <typename T, typename... Args>
	void lazyDeactivate(Args&&... args)
	{
		std::shared_ptr<Layer>layer = std::make_shared<T>(std::forward<Args>(args)...);
		auto p = m_active_layers.find(layer->id());
		if (p != m_active_layers.end())
		{
			m_with_stack.push_back(*p);
			m_active_layers.erase(p);
		}
		// else: deactivate whats not there:
		// nothing to do!
#if DEBUG && VERBOSE
		std::cout << "lazy deactivate layer: " << layer->id() << std::endl;
#endif
	}

public:
	template <typename T, typename... Args>
	void deactivate(Args&&... args)
	{
		std::shared_ptr<Layer>layer = std::make_shared<T>(std::forward<Args>(args)...);
		m_active_layers.erase(layer->id());
#if DEBUG && VERBOSE
		std::cout << "deactivate layer: " << layer->id() << std::endl;
#endif
		notify({layer->id()});
	}

public:
	Context & withl(std::shared_ptr<Layer>&l)
	{
		// build up staple (until function is executed)
		lazyActivateLayer(l);
		return *this;
	}

	template <typename T, typename... Args>
	Context & with(Args&&... args)
	{
		// build up staple (until function is executed)
		lazyActivate<T, Args...>(std::forward<Args>(args)...);
		return *this;
	}

	template <typename T, typename... Args>
	Context & without(Args&&... args)
	{
		// build up staple (until function is executed)
		lazyDeactivate<T, Args...>(std::forward<Args>(args)...);
		return *this;
	}

	Context & operator()(std::function<void()> const & f)
	{
		execHelper(f);

		return *this;
	}

	Context & withl(std::shared_ptr<Layer>&l, std::function<void()> const & f)
	{
		lazyActivateLayer(l);
		execHelper(f);

		return *this;
	}

private:
	typedef std::vector<std::pair<std::string, std::shared_ptr<Layer>>> WithStack;

	void execHelper(std::function<void()> const & f)
	{
		WithStack with_stack = m_with_stack;
		m_with_stack.clear(); // allow with to be called recursively
		// last step, now lets really activate
		Subject::Events to_notify;
		for (auto & s: with_stack)
		{
			to_notify.push_back(s.first);
		}
		notify(to_notify);

		// now do the function call,
		// keep roll back information on the stack
		f();

		// now roll everything back before all those with()
		// and without()
		while(!with_stack.empty())
		{
			auto s = with_stack.back();
			with_stack.pop_back();
			if (!s.second)
			{
				// do not add null pointer
				// but erase layer instead
				m_active_layers.erase(s.first);
			}
			else
			{
				auto it = m_active_layers.insert(s);
				if (!it.second)
				{
					it.first->second = s.second;
				}
			}
		}
		notify(to_notify);
	}

	std::unordered_map<std::string, std::shared_ptr<Layer>> m_active_layers;
	// the with stack holds all layers that were
	// changed in the current .with().with()
	// invocation chain
	WithStack m_with_stack;
};



// standard types

template<typename T>
class ContextualValue : public Observer
{
public:
	typedef T type;

	// not to be constructed yourself
	ContextualValue<T>(KeySet & ks, Context & context_, kdb::Key meta) :
		m_cache(),
		m_ks(ks),
		m_context(context_),
		m_meta(meta),
		m_evaluated_name(m_context.evaluate(m_meta.getString()))
	{
		assert(m_meta.getString()[0] == '/');
		syncCache();  // read what we have in our context
		m_context.attachByName(m_meta.getString(), *this);
	}

	ContextualValue<T>(ContextualValue<T> const & other, KeySet & ks) :
		m_cache(other.m_cache),
		m_ks(ks),
		m_context(other.m_context),
		m_meta(other.m_meta),
		m_evaluated_name(other.m_evaluated_name)
	{
		assert(m_meta.getString()[0] == '/');
		// cache already in sync
		// attach copy, too:
		m_context.attachByName(m_meta.getString(), *this);
	}

public:
	ContextualValue<T> const & operator= (type n)
	{
		m_cache = n;

		return *this;
	}

	type operator ++()
	{
		return ++m_cache;
	}

	type operator ++(int)
	{
		return m_cache++;
	}

	operator type() const
	{
			return m_cache;
	}

	bool operator == (ContextualValue<T> const & other) const
	{
		return m_cache == other.m_cache ;
	}

	type getDefault() const
	{
		return m_meta.getMeta<type>("default");
	}

	bool isEmpty() const
	{
		return !m_ks.lookup(m_meta.getString(), 0);
	}

	/// We allow manipulation of context for const
	/// objects
	Context & context() const
	{
		return const_cast<Context&>(m_context);
	}

	/// Do not inline so that we can use it for debugging
	
	ELEKTRA_INLINE std::string const & getEvaluatedName() const
	{
		return m_evaluated_name;
	}

	// keyset to cache
	void syncCache() const
	{
		kdb::Key found = m_ks.lookup(m_evaluated_name, 0);
		if (found)
		{
			m_cache = found.get<type>();
		}
		else
		{
			m_cache = getDefault();
		}

		/*
		m_context_changed = false;
		m_atomic_context_changed = false;
		m_volatile_context_changed = false;
		*/
#if DEBUG && VERBOSE
		std::cout << "got name: " << m_evaluated_name << " to " << m_cache << std::endl;
#endif
	}

	// cache to keyset
	void syncKeySet() const
	{
#if DEBUG && VERBOSE
		std::cout << "set name: " << m_evaluated_name << " to " << m_cache << std::endl;
#endif
		kdb::Key found = m_ks.lookup(m_evaluated_name, 0);

		if(!found)
		{
			kdb::Key k("user/"+m_evaluated_name, KEY_END);
			k.set<type>(m_cache);
			m_ks.append(k);
		}
		else
		{
			found.set<type>(m_cache);
		}
	}


private:
	virtual void update() const
	{
		std::string evaluated_name = m_context.evaluate(m_meta.getString());
#if DEBUG && VERBOSE
		std::cout << "update " << evaluated_name << " from " << m_evaluated_name << std::endl;
#endif
		if (evaluated_name != m_evaluated_name)
		{
			syncKeySet(); // flush out what currently is in cache
			m_evaluated_name = evaluated_name;
			syncCache();  // read what we have under new context
		}
	}

private:
	mutable type m_cache;
	KeySet & m_ks;
	Context & m_context;
	Key m_meta;
	mutable std::string m_evaluated_name;
};

typedef ContextualValue<uint32_t>Integer;
typedef ContextualValue<bool>Boolean;
typedef ContextualValue<std::string>String;

}

#endif
