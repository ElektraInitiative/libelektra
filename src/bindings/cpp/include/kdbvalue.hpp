#ifndef ELEKTRA_KDBVALUE_HPP
#define ELEKTRA_KDBVALUE_HPP

#include <kdbconfig.h>

#include <set>
#include <map>
#include <string>
#include <vector>
#include <memory>
#include <cassert>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <algorithm>
#include <functional>
#include <unordered_map>

#include <kdbproposal.h>
#include <keyset.hpp>

// #include <kdbprivate.h> // for debugging (to see values of internal structures)

namespace kdb
{


// some widely used interfaces

/**
 * @brief This type is being used as bottom type that always fails.
 */
class none_t
{};

template <>
inline void Key::set(none_t)
{}

template <>
inline none_t Key::get() const
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
	virtual std::string id() const = 0;
	virtual std::string operator()() const = 0;
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
	virtual ~ValueObserver() = 0;
	virtual void updateContext() const = 0;

	typedef std::reference_wrapper<ValueObserver> reference;
};

/**
 * @brief Needed to put a ValueObserver in a map
 *
 * @return Comparision result
 */
bool operator <(ValueObserver const & lhs, ValueObserver const & rhs)
{
	return &lhs < &rhs;
}

inline ValueObserver::~ValueObserver()
{}



class ValueSubject
{
public:
	virtual void notifyInThread() = 0;
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
	typedef std::function<Key()> Func;
	Command(
		ValueSubject const & v_,
		Func & execute_,
		Key & oldKey_,
		Key newKey_ = Key()) :
		v(const_cast<ValueSubject &>(v_)),
		execute(execute_),
		oldKey(oldKey_),
		newKey(newKey_)
	{}

	Key operator()() {return execute();}

	ValueSubject & v; // this pointer
	Func & execute; // to be executed within lock
	Key & oldKey; // null key if initial assignment
	Key  newKey; // new name after assignment
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
	 * NoContext just executes the function and does not
	 * attach/reattach/detach
	 *
	 * @param c the command to apply
	 */
	void execute(Command & c)
	{
		c();
	}
};

/**
 * @brief Implements lookup with spec.
 */
class DefaultGetPolicy
{
public:
	static Key get(KeySet &ks, Key const& spec)
	{
		return ks.lookup(spec, ckdb::KDB_O_SPEC | ckdb::KDB_O_CREATE );
	}
};

/**
 * @brief Implements creating user/ key when key is not found.
 */
class DefaultSetPolicy
{
public:
	static Key set(KeySet &ks, Key const& spec)
	{
		kdb::Key found = ks.lookup(spec.getName(), 0);

		if(!found)
		{
			kdb::Key k("user/"+spec.getName(), KEY_END);
			ks.append(k);
			found = k;
		}

		return found;
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
	void lock() {}
	void unlock() {}
};

/**
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

template < typename Setter1,
	   typename Setter2,
	   typename Setter3,
	   typename Setter4,
	   typename Setter5,
	   typename Setter6
	 >
class PolicySelector : public Discriminator<Setter1,1>,
		       public Discriminator<Setter2,2>,
		       public Discriminator<Setter3,3>,
		       public Discriminator<Setter4,4>,
		       public Discriminator<Setter5,5>,
		       public Discriminator<Setter6,6>
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

template<typename T,
	typename PolicySetter1 = DefaultPolicyArgs,
	typename PolicySetter2 = DefaultPolicyArgs,
	typename PolicySetter3 = DefaultPolicyArgs,
	typename PolicySetter4 = DefaultPolicyArgs,
	typename PolicySetter5 = DefaultPolicyArgs,
	typename PolicySetter6 = DefaultPolicyArgs
	>
class Value :
	public ValueObserver,
	public ValueSubject
{
public:
	typedef T type;

	typedef PolicySelector<
		PolicySetter1,
		PolicySetter2,
		PolicySetter3,
		PolicySetter4,
		PolicySetter5,
		PolicySetter6
		>
		Policies;

	// not to be constructed yourself
	Value<T, PolicySetter1, PolicySetter2, PolicySetter3,
		PolicySetter4, PolicySetter5, PolicySetter6>
		(KeySet & ks, typename Policies::ContextPolicy & context_, kdb::Key spec) :
		m_cache(),
		m_ks(ks),
		m_context(context_),
		m_spec(spec)
	{
		assert(m_spec.getName()[0] == '/');
		m_spec.setMeta("name", m_spec.getName());
		ckdb::elektraKeySetName(*m_spec,
				m_context.evaluate(m_spec.getName()).c_str(),
				KEY_CASCADING_NAME);
		m_context.attachByName(m_spec.getMeta<std::string>("name"), *this);
		syncCache();
	}

	typedef Value<T, PolicySetter1, PolicySetter2, PolicySetter3,
		PolicySetter4, PolicySetter5, PolicySetter6> V;

	V const & operator= (type n)
	{
		static_assert(Policies::WritePolicy::allowed, "read only contextual value");
		m_cache = n;
		syncKeySet();

		return *this;
	}

	type operator ++()
	{
		static_assert(Policies::WritePolicy::allowed, "read only contextual value");
		type ret = ++m_cache;
		syncKeySet();
		return ret;
	}

	type operator ++(int)
	{
		static_assert(Policies::WritePolicy::allowed, "read only contextual value");
		type ret = m_cache++;
		syncKeySet();
		return ret;
	}

	// template < typename = typename std::enable_if< true >::type >
	operator type() const
	{
		return m_cache;
	}

	bool operator == (V const & other) const
	{
		return m_cache == other.m_cache ;
	}


	/**
	 * @return the context bound to the value
	 */
	typename Policies::ContextPolicy & context() const
	{
		/// We allow manipulation of context for const
		/// objects
		return const_cast<typename Policies::ContextPolicy&>(m_context);
	}

	/**
	 * @brief Shortcut for context()
	 *
	 * @see context()
	 */
	typename Policies::ContextPolicy & c() const
	{
		return context();
	}

	/**
	 * @note name is current interpretation
	 * @see getName()
	 *
	 * meta key name contains original name
	 *
	 *
	 * @return Specification Key
	 */
	Key const& getSpec() const
	{
		return m_spec;
	}

	/**
	 * @brief Shortcut to return name
	 *
	 * @return name under contextual interpretation
	 */
	Key const& getName() const
	{
		return m_spec.getName();
	}

	/**
	 * @brief Sync keyset to cache
	 */
	void syncCache() const
	{
		Command::Func fun = [this]
		{
			this->unsafeSyncCache();
			return m_key;
		};
		Key oldKey = m_key.dup();
		Command command(*this, fun, oldKey);
		m_context.execute(command);
	}

	/**
	 * @brief Sync cache to keyset
	 */
	void syncKeySet() const
	{
		Command::Func fun = [this]
		{
			this->unsafeSyncKeySet();
			return m_key;
		};
		Key oldKey = m_key.dup();
		Command command(*this, fun, oldKey);
		m_context.execute(command);
	}

private:

	/**
	 * @brief Execute this method *only* in a Command execution
	 */
	void unsafeSyncCache() const
	{
		m_key = Policies::GetPolicy::get(m_ks, m_spec);
		assert (m_key);
		m_cache = m_key.get<type>();

#if DEBUG && VERBOSE
		std::cout << "got name: " << m_key.getName() << " value: " << m_cache << std::endl;
#endif
	}

	/**
	 * @brief Execute this method *only* in a Command execution
	 */
	void unsafeSyncKeySet() const
	{
		m_key = Policies::SetPolicy::set(m_ks, m_spec);
		assert (m_key);
		m_key.set<type>(m_cache);

#if DEBUG && VERBOSE
		std::cout << "set name: " << found.getName() << " value: " << m_cache << std::endl;
#endif
	}

	/**
	 * @brief Update to new value because of assignment
	 */
	void notifyInThread()
	{
		unsafeSyncCache(); // always called from save context
	}


	virtual void updateContext() const
	{
		std::string evaluated_name = m_context.evaluate(m_spec.getMeta<std::string>("name"));
#if DEBUG && VERBOSE
		std::cout << "update context " << evaluated_name << " from " << m_spec.getName() << std::endl;
#endif
		if (evaluated_name == m_spec.getName()) return; // nothing changed, same name

		Key oldKey;
		Command::Func fun = [this, &evaluated_name]
		{
			this->unsafeSyncKeySet(); // flush out what currently is in cache
			ckdb::elektraKeySetName(*this->m_spec,
					evaluated_name.c_str(),
					KEY_CASCADING_NAME);
			this->unsafeSyncCache();  // read what we have under new context

#if DEBUG && VERBOSE
			std::cout << "set name: " << found.getName() << " value: " << m_cache << std::endl;
#endif
			return m_key;
		};
		Command command(*this, fun, oldKey);
		m_context.execute(command);
	}

private:
	mutable type m_cache;
	KeySet & m_ks;
	typename Policies::ContextPolicy & m_context;
	Key m_spec;
	mutable Key m_key;
};

}

#endif
