#ifndef ELEKTRA_KDBVALUE_HPP
#define ELEKTRA_KDBVALUE_HPP

#ifdef HAVE_KDBCONFIG_H
#include <kdbconfig.h>
#endif

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


namespace kdb
{


// some widely used interfaces

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



// Default Policies for Value

class NoContext
{
public:
	void attachByName(ELEKTRA_UNUSED std::string const & key_name,ELEKTRA_UNUSED  Observer & observer)
	{}
	std::string evaluate(std::string const & key_name) const
	{
		return key_name;
	}
};

/**
 * @brief simply lookup without spec
 */
class DefaultGetPolicy
{
public:
	static Key get(KeySet &ks, Key const& spec)
	{
		return ks.lookup(spec, ckdb::KDB_O_SPEC);
	}
};

/**
 * @brief Implements update when key is not found.
 *
 * The new value always can be written out
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
	public Observer
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
		syncCache();  // read what we have in our context
		m_context.attachByName(m_spec.getMeta<std::string>("name"), *this);
	}

	Value<T, PolicySetter1, PolicySetter2, PolicySetter3,
		PolicySetter4, PolicySetter5, PolicySetter6>
		(Value<T> const & other, KeySet & ks) :
		m_cache(other.m_cache),
		m_ks(ks),
		m_context(other.m_context),
		m_spec(other.m_spec)
	{
		assert(m_spec.getName()[0] == '/');
		// cache already in sync
		// attach copy, too:
		m_context.attachByName(m_spec.getMeta<std::string>("name"), *this);
	}

	typedef Value<T, PolicySetter1, PolicySetter2, PolicySetter3,
		PolicySetter4, PolicySetter5, PolicySetter6> CV;

	CV const & operator= (type n)
	{
		static_assert(Policies::WritePolicy::allowed, "read only contextual value");
		m_cache = n;

		return *this;
	}

	type operator ++()
	{
		static_assert(Policies::WritePolicy::allowed, "read only contextual value");
		return ++m_cache;
	}

	type operator ++(int)
	{
		static_assert(Policies::WritePolicy::allowed, "read only contextual value");
		return m_cache++;
	}

	// template < typename = typename std::enable_if< true >::type >
	operator type() const
	{
			return m_cache;
	}

	bool operator == (CV const & other) const
	{
		return m_cache == other.m_cache ;
	}

	type getDefault() const
	{
		return m_spec.getMeta<type>("default");
	}

	/// We allow manipulation of context for const
	/// objects
	typename Policies::ContextPolicy & context() const
	{
		return const_cast<typename Policies::ContextPolicy&>(m_context);
	}

	Key const& getSpec() const
	{
		return m_spec;
	}

	// keyset to cache
	void syncCache() const
	{
		Key found = Policies::GetPolicy::get(m_ks, m_spec);

		if (found)
		{
			m_cache = found.get<type>();
		}

#if DEBUG && VERBOSE
		std::cout << "got name: " << m_spec.getName() << " to " << m_cache << std::endl;
#endif
	}

	// cache to keyset
	void syncKeySet() const
	{
#if DEBUG && VERBOSE
		std::cout << "set name: " << m_spec.getName() << " to " << m_cache << std::endl;
#endif
		kdb::Key found = Policies::SetPolicy::set(m_ks, m_spec);

		if (found)
		{
			found.set<type>(m_cache);
		}
	}


private:
	virtual void update() const
	{
		// Policies::UpdatePolicy::update(this);
		typename Policies::LockPolicy lock;
		lock.lock();

		std::string evaluated_name = m_context.evaluate(m_spec.getMeta<std::string>("name"));
#if DEBUG && VERBOSE
		std::cout << "update " << evaluated_name << " from " << m_spec.getName() << std::endl;
#endif
		if (evaluated_name != m_spec.getName())
		{
			syncKeySet(); // flush out what currently is in cache
			ckdb::elektraKeySetName(*m_spec,
					evaluated_name.c_str(),
					KEY_CASCADING_NAME);
			syncCache();  // read what we have under new context
		}
		lock.unlock();
	}

private:
	mutable type m_cache;
	KeySet & m_ks;
	typename Policies::ContextPolicy & m_context;
	mutable Key m_spec;
	mutable Key m_key;
};

}

#endif
