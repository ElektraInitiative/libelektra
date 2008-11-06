SCHEME42 = scheme42
AM_CFLAGS = -Wall -I../../include

noinst_PROGRAMS = elektra-grovel
elektra_grovel_SOURCES = elektra-grovel.c
BUILT_SOURCES = elektra-grovel.c

noinst_DATA = elektra-grovel.scm

update-worlds.stamp:
	$(SCHEME42) --load-system universe --update-worlds --exit
	touch $@

%-grovel.c: %-grovel.scm.in update-worlds.stamp
	$(SCHEME42) --load-system universe,grovel --open c-groveller -c "(grovel-file \"$<\" \"$*-grovel.c\")"

%-grovel.scm: %-grovel
	./$*-grovel > $@

clean-local:
	rm -f *-grovel.{scm,c} *.stamp

MAINTAINERCLEANFILES = \
	Makefile.in
