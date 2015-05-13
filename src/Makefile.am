## Process this file with Automake to create Makefile.in

DEFS = -DGWM_APP_name=\"gwm\" -DDEFAULT_GWMPATH=\"@datadir@/@PACKAGE@\"

bin_PROGRAMS = gwm

gwm_SOURCES = \
	active.cc client.cc cursor.cc deco.cc decofunc.cc drawing.cc \
	error.cc event.cc font.cc fsm.cc feedback.cc gwm.cc gwmfunc.cc \
	paint.cc screen.cc wops.cc

DOT_X_FILES = \
	active.x cursor.x decofunc.x drawing.x error.x event.x font.x \
	fsm.x feedback.x gwm.x gwmfunc.x paint.x screen.x wops.x

DOT_DOC_FILES = \
	active.doc cursor.doc decofunc.doc drawing.doc error.doc event.doc \
	font.doc fsm.doc feedback.doc gwm.doc gwmfunc.doc paint.doc \
	screen.doc wops.doc

BUILT_SOURCES = $(DOT_X_FILES) $(DOT_DOC_FILES)

gwm_DEPENDENCIES = $(DOT_X_FILES)

gwm_LDADD = @GUILE_LIBS@ @READLINE_LIBS@ @IMLIB_LIBS@ @XPM_LIB@ @XEXT_LIB@ @x_libs@ 

gwm_LDFLAGS = @x_ldflags@

INCLUDES = @x_cflags@ @GUILE_INCLUDES@ @IMLIB_CFLAGS@

SUFFIXES = .x .doc

if GUILE_V14

.cc.x:
	PATH=.:${PATH} gwm-guile-doc-snarf $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }
.cc.doc:
	PATH=.:${PATH} gwm-guile-doc-snarf $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > /dev/null \
	|| { rm $@; false; }

else

.cc.x:
	guile-snarf $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }
.cc.doc:
	touch $@

endif

gwm-procedures.txt: $(DOT_DOC_FILES)
	cat *.doc > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

CLEANFILES = *.x *.doc

EXTRA_DIST = *.hh *.h guile-func-name-check

