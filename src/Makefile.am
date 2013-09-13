INCLUDES = \
	-I. \
	-I$(top_srcdir)/src \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
	$(PACKAGE_CFLAGS) \
	$(G_CAST_CHECKS)

module_LTLIBRARIES = windowtitle.la

moduledir = $(libdir)/lxpanel/plugins

windowtitle_la_SOURCES = \
	windowtitle.c

windowtitle_la_LIBADD = \
	$(PACKAGE_LIBS)

windowtitle_la_LDFLAGS = \
	-module \
	@LXPANEL_MODULE@
