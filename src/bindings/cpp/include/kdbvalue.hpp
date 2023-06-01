/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBVALUE_HPP
#define ELEKTRA_KDBVALUE_HPP

#ifdef HAVE_KDBCONFIG_H
#include <internal/config.h>
#else
#define DEBUG 0
#define VERBOSE 0
#endif

#include <algorithm>
#include <cassert>
#include <elektra/ease/meta.h>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include <internal/utility/old_helper.h> // for elektraLookupOptions
#include <keyset.hpp>

// #include <kdbprivate.h> // for debugging (to see values of internal structures)

namespace kdb
{


// some widely used interfaces

/**
 * @brief This type is being used as bottom type that always fails.
 */
class none_t
{
};

template <>
inline void Key::set (none_t)
{
}

template <>
inline none_t Key::get () const
{
	none_t ret;
	return ret;
}


/**
 * @brief Base class for all layers.
 */
class Layer
{
public:
	virtual ~Layer (){};
	virtual std::string id () const = 0;
	virtual std::string operator() () const = 0;
};

/**
 * @brief Everything implementing this interface can be used as layer
 *
 * Different from "Layer" objects they will not be constructed on activation
 * but instead only the WrapLayer will be constructed and the wrapped object
 * will be passed along by reference.
 *
 * @note the lifetime must be beyond layer deactivation!
 */
class Wrapped
{
public:
	virtual ~Wrapped (){};
	virtual std::string layerId () const = 0;
	virtual std::string layerVal () const = 0;
};

class WrapLayer : public Layer
{
public:
	explicit WrapLayer (Wrapped const & wrapped) : m_wrapped (wrapped)
	{
	}

	virtual ~WrapLayer (){};

	virtual std::string id () const
	{
		return m_wrapped.layerId ();
	}

	virtual std::string operator() () const
	{
		return m_wrapped.layerVal ();
	}

private:
	Wrapped const & m_wrapped;
};

class KeyValueLayer : public kdb::Layer
{
public:
	KeyValueLayer (std::string key, std::string value) : m_key (std::move (key)), m_value (std::move (value))
	{
	}

	virtual ~KeyValueLayer (){};

	std::string id () const override
	{
		return m_key;
	}
	std::string operator() () const override
	{
		return m_value;
	}

private:
	std::string m_key;
	std::string m_value;
};

/**
 * @brief Base class for values to be observed.
 *
 * updateContext() is called whenever a context tells a value that it
 * should reevaluate its name and update its cache.
 */
class ValueObserver
{
public:
	virtual ~ValueObserver () = 0;
	virtual void updateContext (bool write = true) const = 0;
	virtual kdb::Key getDepKey () const = 0;

	typedef std::reference_wrapper<ValueObserver> reference;
};

/**
 * @brief Needed to put a ValueObserver in a map
 *
 * @return Comparison result
 */
inline bool operator<(ValueObserver const & lhs, ValueObserver const & rhs)
{
	return &lhs < &rhs;
}

inline ValueObserver::~ValueObserver ()
{
}


class ValueSubject
{
public:
	virtual void notifyInThread () = 0;
};

/**
 * @brief Used by contexts for callbacks (to run code using a mutex).
 *
 * Following scenarios are possible:
 * !oldName && !newName: execute code, do nothing else
 * !oldName && newName: attach
 * oldName && newName: reattach
 * oldName == newName: assignment, attach for inter-thread updates
 * oldName && !newName: detach
 */
struct Command
{
public:
	typedef std::pair<std::string, std::string> Pair;
	/**
	 * @brief Typedef for function that returns oldKey, newKey pair
	 */
	typedef std::function<Pair ()> Func;
	Command (ValueSubject const & v_, Func & execute_, bool hasChanged_ = false)
	: v (const_cast<ValueSubject &> (v_)), execute (execute_), hasChanged (hasChanged_), oldKey (), newKey ()
	{
	}

	Pair operator() ()
	{
		return execute ();
	}

	ValueSubject & v;   // this pointer
	Func & execute;	    // to be executed within lock
	bool hasChanged;    // if the value (m_cache) has changed and value propagation is needed
	std::string oldKey; // old name before assignment
	std::string newKey; // new name after assignment
};

// Default Policies for Value

class NoContext
{
public:
	/**
	 * @brief attach a new value
	 *
	 * NoContext will never update anything
	 */
	void attachByName (ELEKTRA_UNUSED std::string const & key_name, ELEKTRA_UNUSED ValueObserver & ValueObserver)
	{
	}

	/**
	 * @brief The evaluated equals the non-evaluated name!
	 *
	 * @return NoContext always returns the same string
	 */
	std::string evaluate (std::string const & key_name) const
	{
		return key_name;
	}

	std::string evaluate (std::string const & key_name,
			      std::function<bool (std::string const &, std::string &, bool in_group)> const &) const
	{
		return key_name;
	}

	/**
	 * @brief (Re)attaches a ValueSubject to a thread or simply
	 *        execute code in a locked section.
	 *
	 * NoContext just executes the function and does not
	 * attach/reattach/detach
	 *
	 * @param c the command to apply
	 */
	void execute (Command & c)
	{
		c ();
	}
};

/**
 * @brief Implements lookup with spec.
 */
class DefaultGetPolicy
{
public:
	static Key get (KeySet & ks, Key const & spec)
	{
		return ks.lookup (spec, ckdb::KDB_O_SPEC | ckdb::KDB_O_CREATE);
	}
};

/**
 * @brief Implements creating user:/ key when key is not found.
 */
class DefaultSetPolicy
{
public:
	static Key set (KeySet & ks, Key const & spec)
	{
		return setWithNamespace (ks, spec, "user:");
	}

	static Key setWithNamespace (KeySet & ks, Key const & spec, std::string const & ns)
	{
		std::string const & name = spec.getName ();
		kdb::Key k (ns + "/" + name.substr (name.find ('/')), KEY_END);
		ks.append (k);

		return k;
	}
};

class DefaultWritePolicy
{
public:
	static const bool allowed = true;
};

class ReadOnlyPolicy
{
public:
	static const bool allowed = false;
};

class DefaultObserverPolicy
{
public:
	typedef double type;
};

class NoLockPolicy
{
public:
	void lock ()
	{
	}
	void unlock ()
	{
	}
};

/*
 * This technique with the PolicySelector and Discriminator is taken
 * from the book  "C++ Templates - The Complete Guide"
 * by David Vandevoorde and Nicolai M. Josuttis, Addison-Wesley, 2002
 * in Chapter 16 Templates and Inheritance: Named Template Arguments
 *
 * The technique allows users of the class Value to use any number
 * and order of policies as desired.
 */


template <typename Base, int D>
class Discriminator : public Base
{
};

template <typename Setter1, typename Setter2, typename Setter3, typename Setter4, typename Setter5, typename Setter6>
class PolicySelector : public Discriminator<Setter1, 1>,
		       public Discriminator<Setter2, 2>,
		       public Discriminator<Setter3, 3>,
		       public Discriminator<Setter4, 4>,
		       public Discriminator<Setter5, 5>,
		       public Discriminator<Setter6, 6>
{
};

class DefaultPolicies
{
public:
	typedef DefaultGetPolicy GetPolicy;
	typedef DefaultSetPolicy SetPolicy;
	typedef NoContext ContextPolicy;
	typedef DefaultWritePolicy WritePolicy;
	typedef DefaultObserverPolicy ObserverPolicy;
	typedef NoLockPolicy LockPolicy;
};

class DefaultPolicyArgs : virtual public DefaultPolicies
{
};


// class templates to override the default policy values

/// Needed by the user to set one of the policies
///
/// @tparam Policy
template <typename Policy>
class GetPolicyIs : virtual public DefaultPolicies
{
public:
	typedef Policy GetPolicy;
};


/// Needed by the user to set one of the policies
///
/// @tparam Policy
template <typename Policy>
class SetPolicyIs : virtual public DefaultPolicies
{
public:
	typedef Policy SetPolicy;
};


/// Needed by the user to set one of the policies
///
/// @tparam Policy
template <typename Policy>
class ContextPolicyIs : virtual public DefaultPolicies
{
public:
	typedef Policy ContextPolicy;
};


/// Needed by the user to set one of the policies
///
/// @tparam Policy
template <typename Policy>
class WritePolicyIs : virtual public DefaultPolicies
{
public:
	typedef Policy WritePolicy;
};


/// Needed by the user to set one of the policies
///
/// @tparam Policy
template <typename Policy>
class ObserverPolicyIs : virtual public DefaultPolicies
{
public:
	typedef Policy ObserverPolicy;
};


/// Needed by the user to set one of the policies
///
/// @tparam Policy
template <typename Policy>
class LockPolicyIs : virtual public DefaultPolicies
{
public:
	typedef Policy LockPolicy;
};


// standard types

template <typename T, typename PolicySetter1 = DefaultPolicyArgs, typename PolicySetter2 = DefaultPolicyArgs,
	  typename PolicySetter3 = DefaultPolicyArgs, typename PolicySetter4 = DefaultPolicyArgs,
	  typename PolicySetter5 = DefaultPolicyArgs, typename PolicySetter6 = DefaultPolicyArgs>
class Value : public ValueObserver, public ValueSubject, public Wrapped
{
public:
	typedef T type;

	typedef PolicySelector<PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5, PolicySetter6> Policies;

	Value (const Value &) = default;

	// not to be constructed yourself
	Value<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5, PolicySetter6> (
		KeySet & ks, typename Policies::ContextPolicy & context_, kdb::Key spec)
	: m_cache (), m_hasChanged (false), m_ks (ks), m_context (context_), m_spec (spec)
	{
		assert (m_spec.getName ()[0] == '/' && "spec keys are not yet supported");
		m_context.attachByName (m_spec.getName (), *this);
		Command::Func fun = [this] () -> Command::Pair {
			auto evaluatedName = m_context.evaluate (m_spec.getName ());
			evaluatedName = evaluatedName == "/%" ? "/" : evaluatedName;
			this->unsafeUpdateKeyUsingContext (evaluatedName);
			this->unsafeSyncCache (); // set m_cache
			return std::make_pair ("", m_key.getName ());
		};
		Command command (*this, fun);
		m_context.execute (command);
	}

	~Value<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5, PolicySetter6> ()
	{
		Command::Func fun = [this] () -> Command::Pair {
			std::string oldName = m_key.getName ();
			m_key = static_cast<ckdb::Key *> (nullptr);
			// after destructor we do not need to care about
			// invariant anymore. But we need to care about
			// thread safe m_key.
			return std::make_pair (oldName, "");
		};
		Command command (*this, fun);
		m_context.execute (command);
	}

	typedef Value<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5, PolicySetter6> V;

	V const & operator= (type n)
	{
		static_assert (Policies::WritePolicy::allowed, "read only contextual value");
		m_cache = n;
		m_hasChanged = true;
		syncKeySet ();

		return *this;
	}

	type operator++ ()
	{
		static_assert (Policies::WritePolicy::allowed, "read only contextual value");
		type ret = ++m_cache;
		m_hasChanged = true;
		syncKeySet ();
		return ret;
	}

	type operator++ (int)
	{
		static_assert (Policies::WritePolicy::allowed, "read only contextual value");
		type ret = m_cache++;
		m_hasChanged = true;
		syncKeySet ();
		return ret;
	}

	type operator-- ()
	{
		static_assert (Policies::WritePolicy::allowed, "read only contextual value");
		type ret = --m_cache;
		m_hasChanged = true;
		syncKeySet ();
		return ret;
	}

	type operator-- (int)
	{
		static_assert (Policies::WritePolicy::allowed, "read only contextual value");
		type ret = m_cache--;
		m_hasChanged = true;
		syncKeySet ();
		return ret;
	}

	V & operator= (V const & rhs)
	{
		static_assert (Policies::WritePolicy::allowed, "read only contextual value");
		if (this != &rhs)
		{
			m_cache = rhs;
			m_hasChanged = true;
			syncKeySet ();
		}
		return *this;
	}

#define ELEKTRA_DEFINE_OPERATOR(op)                                                                                                        \
	V & operator op (type const & rhs)                                                                                                 \
	{                                                                                                                                  \
		static_assert (Policies::WritePolicy::allowed, "read only contextual value");                                              \
		m_cache op rhs;                                                                                                            \
		m_hasChanged = true;                                                                                                       \
		syncKeySet ();                                                                                                             \
		return *this;                                                                                                              \
	}

	ELEKTRA_DEFINE_OPERATOR (-=)
	ELEKTRA_DEFINE_OPERATOR (+=)
	ELEKTRA_DEFINE_OPERATOR (*=)
	ELEKTRA_DEFINE_OPERATOR (/=)
	ELEKTRA_DEFINE_OPERATOR (%=)
	ELEKTRA_DEFINE_OPERATOR (^=)
	ELEKTRA_DEFINE_OPERATOR (&=)
	ELEKTRA_DEFINE_OPERATOR (|=)

#undef ELEKTRA_DEFINE_OPERATOR

	type operator- () const
	{
		return -m_cache;
	}

	type operator~() const
	{
		return ~m_cache;
	}

	type operator!() const
	{
		return !m_cache;
	}

	// type conversion
	operator type () const
	{
		return m_cache;
	}

	/**
	 * @return the context bound to the value
	 */
	typename Policies::ContextPolicy & context () const
	{
		/// We allow manipulation of context for const
		/// objects
		return const_cast<typename Policies::ContextPolicy &> (m_context);
	}

	/**
	 * @brief Shortcut for context()
	 *
	 * @see context()
	 */
	typename Policies::ContextPolicy & c () const
	{
		return context ();
	}

	/**
	 * @return Specification Key
	 */
	Key const & getSpec () const
	{
		return m_spec;
	}

	/**
	 * @brief Returns the current name of contextual value
	 *
	 * @return name under contextual interpretation
	 */
	std::string getName () const
	{
		return m_key.getName ();
	}

	std::string layerId () const override
	{
		const Key meta = m_spec.getMeta<const Key> ("layer/name");
		if (meta) return meta.getString ();
		return m_spec.getBaseName ();
	}

	std::string layerVal () const override
	{
		return m_key.getString ();
	}


	/**
	 * @brief Sync key(set) to cache
	 */
	void syncCache () const
	{
		Command::Func fun = [this] () -> Command::Pair {
			std::string const & oldKey = m_key.getName ();
			this->unsafeLookupKey ();
			this->unsafeSyncCache ();
			return std::make_pair (oldKey, m_key.getName ());
		};
		Command command (*this, fun);
		m_context.execute (command);
	}

	/**
	 * @brief Sync cache to key(set)
	 */
	void syncKeySet () const
	{
		Command::Func fun = [this] () -> Command::Pair {
			std::string const & oldKey = m_key.getName ();
			this->unsafeSyncKeySet ();
			return std::make_pair (oldKey, m_key.getName ());
		};
		Command command (*this, fun, m_hasChanged);
		m_context.execute (command);
	}

private:
	void unsafeUpdateKeyUsingContext (std::string const & evaluatedName) const
	{
		Key spec (m_spec.dup ());
		spec.setName (evaluatedName);
		m_key = Policies::GetPolicy::get (m_ks, spec);
		assert (m_key);
	}

	void unsafeLookupKey () const
	{
		// Key spec (m_spec.dup ());
		// spec.setName (m_context.evaluate(m_spec.getName()));
		// m_key = Policies::GetPolicy::get (m_ks, spec);
		m_key = Policies::GetPolicy::get (m_ks, m_key);
		assert (m_key);
	}

	/**
	 * @brief Unsafe: Execute this method *only* in a Command execution
	 */
	void unsafeSyncCache () const
	{
		assert (m_key);

#if DEBUG
		std::cout << "will get name: " << m_key.getName () << " value: " << m_key.getString () << std::endl;
#endif

		m_cache = m_key.get<type> ();
	}

	/**
	 * @brief Execute this method *only* in a Command execution
	 */
	void unsafeSyncKeySet () const
	{
		if (m_hasChanged && m_key.getNamespace () == ElektraNamespace::DEFAULT)
		{
			m_hasChanged = false;
			Key spec (m_spec.dup ());
			spec.setName (m_key.getName ());
			m_key = Policies::SetPolicy::set (m_ks, spec);
		}
		assert (m_key);
		m_key.set<type> (m_cache);

#if DEBUG
		std::cout << "set name: " << m_key.getName () << " value: " << m_key.getString () << std::endl;
#endif
	}

	/**
	 * @brief Update to new value because of assignment
	 */
	void notifyInThread () override
	{
		unsafeSyncCache (); // always called from save context
	}


	virtual void updateContext (bool write) const override
	{
		std::string evaluatedName = m_context.evaluate (m_spec.getName ());
#if DEBUG
		std::cout << "update context " << evaluatedName << " from " << m_spec.getName () << " with write " << write << std::endl;
#endif

		Command::Func fun = [this, &evaluatedName, write] () -> Command::Pair {
			std::string oldKey = m_key.getName ();
			if (write && "default:" + evaluatedName == oldKey)
			{
				// nothing changed, same name
				return std::make_pair (evaluatedName, evaluatedName);
			}

			if (write)
			{
				this->unsafeSyncKeySet (); // flush out what currently is in cache
			}

			this->unsafeUpdateKeyUsingContext (evaluatedName);
			this->unsafeSyncCache (); // read what we have under new context

			return std::make_pair (oldKey, m_key.getName ());
		};
		Command command (*this, fun);
		m_context.execute (command);
	}

	virtual kdb::Key getDepKey () const override
	{
		kdb::Key dep ("/" + layerId (), KEY_END);
		// rename to /layer/order
		const Key meta = m_spec.getMeta<const Key> ("layer/order");
		if (meta)
		{
			dep.setMeta ("order", meta.getString ());
		}
		m_context.evaluate (m_spec.getName (), [&] (std::string const & current_id, std::string &, bool) {
#if DEBUG
			std::cout << "add dep " << current_id << " to " << dep.getName () << std::endl;
#endif
			ckdb::elektraMetaArrayAdd (*dep, "dep", ("/" + current_id).c_str ());
			return false;
		});
		return dep;
	}

private:
	/**
	 * @brief A transient mutable cache for very fast read-access.
	 */
	mutable type m_cache;

	/**
	 * @brief triggers transition from transient to persistent keys
	 * @retval true if m_cache was changed
	 */
	mutable bool m_hasChanged;

	/**
	 * @brief Reference to the keyset in use
	 *
	 * only accessed using
	 * Command, that might be multi-thread safe depending on
	 * ContextPolicyIs
	 */
	KeySet & m_ks;

	/**
	 * @brief The context that might be
	 *
	 * - thread safe
	 * - allow COP
	 */
	typename Policies::ContextPolicy & m_context;

	/**
	 * @brief The specification key
	 *
	 * Is only read and will not be changed.
	 *
	 * Might start with / or with spec:/ (not implemented yet)
	 */
	Key m_spec;

	/**
	 * @brief The current key the Value is bound to.
	 *
	 * @invariant: Is never a null key
	 */
	mutable Key m_key;
};

template <typename T, typename PolicySetter1, typename PolicySetter2, typename PolicySetter3, typename PolicySetter4,
	  typename PolicySetter5, typename PolicySetter6>
std::ostream & operator<< (std::ostream & os,
			   Value<T, PolicySetter1, PolicySetter2, PolicySetter3, PolicySetter4, PolicySetter5, PolicySetter6> const & v)
{
	os << static_cast<T> (v);
	return os;
}
} // namespace kdb

#endif
