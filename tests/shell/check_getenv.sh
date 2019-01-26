@INCLUDE_COMMON@

echo
echo ELEKTRA GETENV tests
echo

kdb elektrify-getenv getenv KDB
succeed_if "could not getenv"

kdb elektrify-getenv getenv PATH
succeed_if "could not getenv"

kdb elektrify-getenv getenv CHECK_VERSION
succeed_if "could not getenv"

end_script
