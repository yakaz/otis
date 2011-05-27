ACLOCAL_FLAGS = -I m4
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = include src ebin priv testsuite

EXTRA_DIST = debian/changelog						\
	     debian/compat						\
	     debian/control						\
	     debian/copyright						\
	     debian/rules
if DIST_REPOREV
EXTRA_DIST += REPOREV
endif

distdir = $(PACKAGE)-$(VERSION)$(APP_DISTSUFFIX)
