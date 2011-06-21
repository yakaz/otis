# $Id: Makefile.am 11510 2011-05-31 07:28:27Z jean.sebastien.pedron $

ETEST         = etest
TPL_PL        = template.pl
TPL_ERL       = template.erl
COVER_TO_HTML = cover_to_html.sh

TPL_FILES = test_types.erl test_vars.erl test_conf.erl test_rewrites.erl test_yakaz.erl

ERL_TESTS = types vars conf rewrites yakaz

TESTS = dialyzer $(ERL_TESTS)

TESTS_ENVIRONMENT = ERL_LIBS=$(ERL_LIBS)

EXTRA_DIST = $(TPL_PL) $(TPL_ERL) $(ERL_FILES) eunit_lib.erl 		\
	     eunit_tty.erl $(COVER_TO_HTML) mod_coverage.escript data

CLEANFILES = $(ERL_TESTS) $(TPL_FILES) eunit_lib.beam eunit_tty.beam 	\
	     erl_crash.dump cover_*.html cover_*.out cover_*.percent	\
	     index*.html *.coverdata

# check-local is executed after check. We use this to output cover
# results.
check-local:
	@$(srcdir)/$(COVER_TO_HTML) $(TESTS)

$(ERL_TESTS):
	@(test ! -e eunit_lib.beam 					\
	  -o $(srcdir)/eunit_lib.erl -nt eunit_lib.beam &&		\
	  echo "Create: eunit_lib" &&					\
	  $(ERLC) $(srcdir)/eunit_lib.erl) || :
	@(test ! -e eunit_tty.beam 					\
	  -o $(srcdir)/eunit_tty.erl -nt eunit_tty.beam &&		\
	  echo "Create: eunit_tty" &&					\
	  $(ERLC) $(srcdir)/eunit_tty.erl) || :
	@(test ! -e $@ -o $(ETEST) -nt $@ && echo "Create: $@" &&	\
	  cp $(ETEST) $@ && chmod 755 $@) || :
	@mod=test_$@.erl; rebuild=no;					\
	 if test ! -e $$mod; then					\
	   rebuild="$$mod not_found";					\
	 elif test $(srcdir)/$(TPL_PL) -nt $$mod; then			\
	   rebuild="$(TPL_PL) newer";					\
	 elif test $(srcdir)/$(TPL_ERL) -nt $$mod; then			\
	   rebuild="$(TPL_ERL) newer";					\
	 else								\
	   for file in $(srcdir)/data/$@/*.pattern; do			\
	     if test $$file -nt $$mod; then				\
	       rebuild="$$file newer";					\
	     fi;							\
	   done;							\
	 fi;								\
	 if test "$$rebuild" != "no"; then				\
	 echo "Create: $$mod";						\
	   env								\
	    srcdir=$(srcdir)						\
	    builddir=$(builddir)					\
	    top_srcdir=$(top_srcdir)					\
	    top_builddir=$(top_builddir)				\
	    $(srcdir)/$(TPL_PL) $(srcdir)/$(TPL_ERL) $(srcdir)/data/$@	\
	    > $$mod;							\
	 fi

.PHONY: $(ERL_TESTS)
