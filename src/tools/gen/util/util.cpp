#from support.cpp import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings

@@staticmethod
@def generateenum(support, parameters)
@for $key, $info in $parameters.items()
@if $support.isenum(info):
/**
 * Class enum of $key
 */
enum class $support.typeof(info)
{
@for $enum in $support.enumval(info)
	$enum,
@end for
};

/** \brief Convert enum to string
 *
 * \return string that holds value of enum
 * \param e the enum that should be converted
 */
template <>
inline void Key::set($support.enumname(info) e)
{
	switch(e)
	{
@for $enum in $support.enumval(info)
	case $support.typeof(info)::$enum: setString("$enum"); break;
@end for
	}
}

/** \brief Convert enum from string
 *
 * \return enum from string s or default value
 * \param s the string that should be converted
 */
template <>
inline $support.enumname(info) Key::get() const
{
	$support.typeof(info) ret $support.valof(info)
@for $enum in $support.enumval(info)
	if(getString() == "$enum")
		ret = $support.typeof(info)::$enum;
@end for
	return ret;
}

@end if
@end for
@end def




@@staticmethod
@def generatebool(support)
/** \brief Convert bool to string
 *
 * \return string that holds value of bool
 * \param e the bool that should be converted
 */
template <>
inline void Key::set(bool e)
{
	if(e)
	{
		setString("true");
	}
	else
	{
		setString("false");
	}
}

/** \brief Convert bool from string
 *
 * \return bool from string s or default value
 * \param s the string that should be converted
 */
template <>
inline bool Key::get() const
{
	bool ret = false;
	if(getString() == "${support.trueval()[0]}")
		ret = true;
@for $b in $support.trueval()[1:]
	else if(getString() == "$b")
		ret = true;
@end for
	return ret;
}
@end def


@@staticmethod
@def generateForwardDecl(support, hierarchy)
@if not hierarchy.children
@return
@end if

@for n in hierarchy.name.split('/')[1:-1]
namespace ${support.nsnpretty($n)}
{
@end for

class ${hierarchy.prettyclassname($support)};

@for n in hierarchy.name.split('/')[1:-1]
}
@end for

@for $child in hierarchy.children
$cpp_util.generateForwardDecl(support, child)
@end for

@end def

@@staticmethod
@def generateGetBySpec(support, key, info)
@if len($support.override(info)) > 0
// override
	kdb::Key found = ks.lookup("${support.override(info)[0]}", 0);
	if (found)
	{
		value = found.get<$support.typeof(info)>();
		$support.generateotransform($info, 0);
	}
@set $oa = $support.override(info)[1:]
@for $o in oa
	found = ks.lookup("$o", 0);
	if (found)
	{
		value = found.get<$support.typeof(info)>();
		$support.generateotransform($info, $oa.index($o));
	}
@end for
	// now the key itself
	found = ks.lookup("$key", 0);
@else
kdb::Key found = ks.lookup("$key", 0);
@end if
	if(found)
	{
		return found.get<$support.typeof(info)>();
	}

@set $fa = $support.fallback(info)
@if len(fa) > 0
	// fallback
@for $f in $fa
	found = ks.lookup("$f", 0);
	if (found)
	{
		value = found.get<$support.typeof(info)>();
		$support.generateftransform($info, $fa.index($f));
	}
@end for
@end if
@end def
