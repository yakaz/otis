erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_otis)/src

dist_erlsrc_DATA = otis_codegen.hrl					\
		   otis_app.erl						\
		   otis_sup.erl						\
		   otis.erl						\
		   otis_conf.erl					\
		   otis_var.erl						\
		   otis_tpl.erl						\
		   otis_utils.erl					\
		   ops/otis_op_log.erl					\
		   ops/otis_op_eq.erl					\
		   ops/otis_op_match.erl				\
		   ops/otis_op_set.erl					\
		   ops/otis_op_subst.erl				\
		   ops/otis_op_not.erl					\
		   ops/otis_op_all.erl					\
		   ops/otis_op_any.erl					\
		   ops/otis_op_goto.erl					\
		   ops/otis_op_response.erl				\
		   types/otis_type_int.erl				\
		   types/otis_type_port.erl

if HAVE_YAWS
dist_erlsrc_DATA += api/otis_yaws_mod_rewrite.erl
endif

CLEANFILES = Emakefile

all-local: Emakefile
	@env ERL_LIBS=$(ERL_LIBS) $(ERL) +B -noshell			\
		-pa @top_builddir@/ebin					\
		-eval "case make:all() of				\
		         up_to_date ->					\
			   halt(0);					\
		         Err ->						\
		           io:format(\"~p~n\", [Err]),			\
		           halt(1)					\
		       end"

# Emakefile is generated automatically with the source files declared above.
Emakefile: Makefile.am Emakefile.in
	@echo "Generate: $@"
	@$(AWK) -v src='$(dist_erlsrc_DATA)'				\
	"{ print }							\
	/% DO NOT EDIT: Modules list is generated/ {			\
		indent = index(\$$0, \"%\");				\
		indent = substr(\$$0, 1, indent - 1);			\
		split(src, files);					\
		files_count = 0;					\
		for (file in files) {					\
			files_count++					\
		}							\
		mods_count = 0;						\
		for (i = 1; i <= files_count; i++) {			\
			if (files[i] !~ /\.erl$$/)			\
				continue;				\
			mods[++mods_count] = files[i]			\
		}							\
		for (i = 1; i <= mods_count; i++) {			\
			mod = mods[i];					\
			sub(/\.erl/, \"\", mod);			\
			if (i != mods_count)				\
				print indent \"'$(srcdir)/\" mod \"',\";\
			else						\
				print indent \"'$(srcdir)/\" mod \"'\"	\
		}							\
	}" Emakefile.in > $@
