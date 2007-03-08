# $Id$

EXTRA_DIST = kdbd.in

elektraenvdir = $(sysconfdir)/profile.d
elektrainitdir = $(sysconfdir)/init.d
elekscriptdir = $(docdir)/scripts

dist_elektraenv_SCRIPTS = elektraenv.sh
elektrainit_SCRIPTS = kdbd
dist_elekscript_DATA = benchmark-createtree convert-hosts convert-inittab \
  convert-users convert-fstab convert-hwconfKudzu convert-network \
  convert-xfree update-backend

nodist_elekscript_DATA = kdbd

# substitute the paths and the program name
kdbd: kdbd.in ../config.status
	kdbdname=`echo kdbd | sed 's|^.*/||;$(transform)'`; \
	sed -e "s:[@]localstatedir[@]:$(localstatedir):g" \
		-e "s:[@]sbindir[@]:$(sbindir):" \
		-e "s:[@]kdbdname[@]:$$kdbdname:g" \
		$(srcdir)/kdbd.in > kdbd

DISTCLEANFILES = kdbd
