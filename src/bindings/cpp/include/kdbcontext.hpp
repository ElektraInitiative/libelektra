/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBCONTEXT_HPP
#define ELEKTRA_KDBCONTEXT_HPP

#include <elektra/ease/meta.h>
#include <kdbvalue.hpp>

#include <cassert>
#include <functional>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace kdb
{

class Subject
{
public:
	virtual ~Subject () = 0;
	typedef std::vector<std::string> Events;
	virtual void attachObserver (ValueObserver &);
	virtual void attachObserverByEvent (std::string const & event, ValueObserver &);

	/**
	 * @brief Notify all with given events
	 *
	 * @param events the event to be notify (observers need to listen to)
	 */
	virtual void notifyByEvents (Events const & events) const;

	/**
	 * @brief Notify by every event.
	 *
	 * Calls notifyByEvents () with every possible event.
	 */
	virtual void notifyAllEvents () const;

	/**
	 * @brief Notify on KeySet update.
	 *
	 * Not only notify keys that have a layer to update, because
	 * every value might be updated now.
	 */
	virtual void notifyKeySetUpdate () const;

protected:
	Subject ();

private:
	typedef std::set<ValueObserver::reference> ObserverSet;
	ObserverSet m_observers;
	mutable std::unordered_map<std::string, ObserverSet> m_events;
};

inline Subject::Subject ()
{
}

inline Subject::~Subject ()
{
}

inline void Subject::attachObserver (ValueObserver & observer)
{
	m_observers.insert (std::ref (observer));
}

inline void Subject::attachObserverByEvent (std::string const & event, ValueObserver & observer)
{
	auto it = m_events.find (event);
	if (it == m_events.end ())
	{
		// add first observer for that event
		// (creating a new vector)
		ObserverSet & os = m_events[event];
		os.insert (std::ref (observer));
	}
	else
	{
		it->second.insert (std::ref (observer));
	}
}

inline void Subject::notifyByEvents (Events const & events) const
{
	ObserverSet os;
	for (auto & e : events)
	{
		auto it = m_events.find (e);
		if (it != m_events.end ())
		{
			for (auto & o : it->second)
			{
				os.insert (o); // (discarding duplicates)
			}
		}
#if DEBUG
		else
		{
			std::cout << "Trying to notify " << e << " but event does not exist" << std::endl;
		}
#endif
	}
	// now call any observer exactly once
	for (auto & o : os)
	{
		o.get ().updateContext ();
	}
}

inline void Subject::notifyAllEvents () const
{
	Events events;
	for (auto & o : m_events)
	{
		events.push_back (o.first);
	}
	notifyByEvents (events);
}

inline void Subject::notifyKeySetUpdate () const
{
	KeySet deps;
	size_t i = 0;
	for (auto & o : m_observers)
	{
		Key dep (o.get ().getDepKey ());
		dep.set<size_t> (i);
		++i;
		deps.append (dep);
	}

	std::vector<ckdb::Key *> ordered;
	ordered.resize (deps.size ());
	int ret = elektraSortTopology (deps.getKeySet (), &ordered[0]);
	if (ret == 0) throw std::runtime_error ("Cycle in layer dependencies");
	if (ret == -1) throw std::logic_error ("elektraSortTopology used wrongly");

	for (auto & o : ordered)
	{
		std::next (m_observers.begin (), Key (o).get<size_t> ())->get ().updateContext (false);
	}
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
	Context () : m_active_layers ()
	{
	}

	virtual void execute (Command & c)
	{
		c ();
	}

	/**
	 * Lookup value for a current active layer
	 *
	 * @param layer the name of the requested layer
	 * @return the layer
	 */
	std::string operator[] (std::string const & layer) const
	{
		auto f = m_active_layers.find (layer);
		if (f != m_active_layers.end ())
		{
			assert (f->second && "no null pointers in active_layers");
			return (*f->second) ();
		}
		return ""; // this line is surprisingly expensive
	}

	/**
	 * @return size of all current layers (to be used with operator[])
	 */
	size_t size () const
	{
		return m_active_layers.size ();
	}

	/**
	 * Attach observer using to all events given by
	 * its specification (name)
	 *
	 * @param key_name the name with placeholders to be used for attaching
	 * @param observer the observer to attach to
	 */
	void attachByName (std::string const & key_name, ValueObserver & observer)
	{
		this->attachObserver (observer);
		evaluate (key_name, [&] (std::string const & current_id, std::string &, bool) {
			this->attachObserverByEvent (current_id, observer);
			return false;
		});
	}

	/**
	 * Evaluate a specification (name) and return
	 * a key name under current context
	 *
	 * @param key_name the name with placeholders to be evaluated
	 */
	std::string evaluate (std::string const & key_name) const
	{
		return evaluate (key_name, [&] (std::string const & current_id, std::string & ret, bool in_group) {
			auto f = m_active_layers.find (current_id);
			bool left_group = true;
			if (f != m_active_layers.end ())
			{
				assert (f->second && "no null pointers in active_layers");
				std::string r = (*f->second) ();
				if (!r.empty ())
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
	std::string evaluate (std::string const & key_name,
			      std::function<bool (std::string const &, std::string &, bool in_group)> const & on_layer) const
	{
		size_t const & s = key_name.size ();
		std::string ret;
		std::string current_id;
		bool capture_id = false; // we are currently within a % block (group or single layer)
		bool left_group = false; // needed to omit layers that do not matter in a group anymore
		bool is_in_group = false;

		// heuristic how much too allocate
		ret.reserve (s * 2);
		current_id.reserve (32);

		for (std::string::size_type i = 0; i < s; ++i)
		{
			if (key_name[i] == '%')
			{
				if (capture_id)
				{
					// finish capturing
					if (!left_group)
					{
						on_layer (current_id, ret, is_in_group);
					}
					current_id.clear ();
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
				left_group = on_layer (current_id, ret, true);
				if (!is_in_group && left_group)
				{
					ret += "%"; // empty groups
				}
				else
				{
					is_in_group = true;
				}
				current_id.clear ();
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

protected:
	// activates layer, records it, but does not notify
	template <typename T, typename... Args>
	void lazyActivate (Args &&... args)
	{
		std::shared_ptr<Layer> layer = std::make_shared<T> (std::forward<Args> (args)...);
		lazyActivateLayer (layer);
	}

	// needed for with
	void lazyActivateLayer (std::shared_ptr<Layer> const & layer)
	{
		std::string const & id = layer->id (); // optimisation
		auto p = m_active_layers.emplace (std::make_pair (id, layer));
		if (!p.second)
		{
			m_with_stack.push_back (*p.first);
			p.first->second = layer; // update
		}
		else
		{
			// no layer was not active before, remember that
			m_with_stack.push_back (std::make_pair (id, std::shared_ptr<Layer> ()));
		}
#if DEBUG
		std::cout << "lazy activate layer: " << id << std::endl;
#endif
	}

	void clearAllLayer ()
	{
		m_active_layers.clear ();
	}

	// needed for global activation
	void activateLayer (std::shared_ptr<Layer> const & layer)
	{
		auto p = m_active_layers.emplace (std::make_pair (layer->id (), layer));
		if (!p.second)
		{
			p.first->second = layer; // update
		}

		notifyByEvents ({ layer->id () });

#if DEBUG
		std::cout << "activate layer: " << layer->id () << std::endl;
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
	std::shared_ptr<Layer> activate (Args &&... args)
	{
		std::shared_ptr<Layer> layer = std::make_shared<T> (std::forward<Args> (args)...);
		activateLayer (layer);
		return layer;
	}

	std::shared_ptr<Layer> activate (Wrapped const & value)
	{
		std::shared_ptr<Layer> layer = std::make_shared<WrapLayer> (value);
		activateLayer (layer);
		return layer;
	}

	std::shared_ptr<Layer> activate (std::string key, std::string value)
	{
		std::shared_ptr<Layer> layer = std::make_shared<KeyValueLayer> (key, value);
		activateLayer (layer);
		return layer;
	}

protected:
	template <typename T, typename... Args>
	void lazyDeactivate (Args &&... args)
	{
		std::shared_ptr<Layer> layer = std::make_shared<T> (std::forward<Args> (args)...);
		lazyDeactivateLayer (layer);
	}

	void lazyDeactivateLayer (std::shared_ptr<Layer> const & layer)
	{
		auto p = m_active_layers.find (layer->id ());
		if (p != m_active_layers.end ())
		{
			m_with_stack.push_back (*p);
			m_active_layers.erase (p);
		}
// else: deactivate whats not there:
// nothing to do!
#if DEBUG
		std::cout << "lazy deactivate layer: " << layer->id () << std::endl;
#endif
	}

	void deactivateLayer (std::shared_ptr<Layer> const & layer)
	{
		m_active_layers.erase (layer->id ());

#if DEBUG
		std::cout << "deactivate layer: " << layer->id () << std::endl;
#endif
	}

public:
	template <typename T, typename... Args>
	std::shared_ptr<Layer> deactivate (Args &&... args)
	{
		std::shared_ptr<Layer> layer = std::make_shared<T> (std::forward<Args> (args)...);
		deactivateLayer (layer);
		notifyByEvents ({ layer->id () });
		return layer;
	}

	std::shared_ptr<Layer> deactivate (Wrapped const & value)
	{
		std::shared_ptr<Layer> layer = std::make_shared<WrapLayer> (value);
		deactivateLayer (layer);
		notifyByEvents ({ layer->id () });
		return layer;
	}

	std::shared_ptr<Layer> deactivate (std::string key, std::string value)
	{
		std::shared_ptr<Layer> layer = std::make_shared<KeyValueLayer> (key, value);
		deactivateLayer (layer);
		notifyByEvents ({ layer->id () });
		return layer;
	}

public:
	Context & withl (std::shared_ptr<Layer> & l)
	{
		// build up staple (until function is executed)
		lazyActivateLayer (l);
		return *this;
	}

	template <typename T, typename... Args>
	Context & with (Args &&... args)
	{
		// build up staple (until function is executed)
		lazyActivate<T, Args...> (std::forward<Args> (args)...);
		return *this;
	}

	template <typename T, typename... Args>
	Context & without (Args &&... args)
	{
		// build up staple (until function is executed)
		lazyDeactivate<T, Args...> (std::forward<Args> (args)...);
		return *this;
	}

	Context & operator() (std::function<void ()> const & f)
	{
		execHelper (f);

		return *this;
	}

	Context & withl (std::shared_ptr<Layer> & l, std::function<void ()> const & f)
	{
		lazyActivateLayer (l);
		execHelper (f);

		return *this;
	}

private:
	typedef std::vector<std::pair<std::string, std::shared_ptr<Layer>>> WithStack;

	void execHelper (std::function<void ()> const & f)
	{
		WithStack with_stack = m_with_stack;
		m_with_stack.clear (); // allow with to be called recursively
		// last step, now lets really activate
		Subject::Events to_notify;
		for (auto & s : with_stack)
		{
			to_notify.push_back (s.first);
		}
		notifyByEvents (to_notify);

		// now do the function call,
		// keep roll back information on the stack
		f ();

		// now roll everything back before all those with()
		// and without()
		while (!with_stack.empty ())
		{
			auto s = with_stack.back ();
			with_stack.pop_back ();
			if (!s.second)
			{
				// do not add null pointer
				// but erase layer instead
				m_active_layers.erase (s.first);
			}
			else
			{
				auto it = m_active_layers.insert (s);
				if (!it.second)
				{
					it.first->second = s.second;
				}
			}
		}
		notifyByEvents (to_notify);
	}

	std::unordered_map<std::string, std::shared_ptr<Layer>> m_active_layers;
	// the with stack holds all layers that were
	// changed in the current .with().with()
	// invocation chain
	WithStack m_with_stack;
};

template <typename T, typename PolicySetter1 = DefaultPolicyArgs, typename PolicySetter2 = DefaultPolicyArgs,
	  typename PolicySetter3 = DefaultPolicyArgs, typename PolicySetter4 = DefaultPolicyArgs,
	  typename PolicySetter5 = DefaultPolicyArgs>
using ContextualValue = Value<T, ContextPolicyIs<Context>, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5>;

typedef ContextualValue<uint32_t> Integer;
typedef ContextualValue<bool> Boolean;
typedef ContextualValue<std::string> String;

} // namespace kdb

#endif
