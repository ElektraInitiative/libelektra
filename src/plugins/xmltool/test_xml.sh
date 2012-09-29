

#############
# xml tests
#############

#echo "testing xml importing and exporting"
#
#$KDB import fstab.xml
#succeed_if "importing fstab.xml failed"
#
#$KDB export user/tests/fstab > fstab-gen.xml
#succeed_if "exporting user/tests/fstab failed"
#
#diff fstab-gen.xml fstab-cmp.xml
#succeed_if "xml files are not the same"
#
#rm fstab-gen.xml
#succeed_if "could not rm key-gen.xml"
#
#$KDB rm -r user/tests/fstab
