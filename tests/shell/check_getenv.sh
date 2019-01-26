@INCLUDE_COMMON@

echo
echo ELEKTRA GETENV tests
echo

"$KDB" elektrify-getenv getenv KDB
succeed_if "could not getenv"

"$KDB" elektrify-getenv getenv PATH
succeed_if "could not getenv"

"$KDB" elektrify-getenv getenv CHECK_VERSION
succeed_if "could not getenv"

end_script
