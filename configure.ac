AC_PREREQ(2.57)
AC_INIT(lxpanel-window-title, 0.1)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE
AC_PROG_INTLTOOL
AM_MAINTAINER_MODE

AC_ISC_POSIX
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LIBTOOL

AC_HEADER_STDC
# Checks for libraries.
pkg_modules="glib-2.0 >= 2.6.0 \
             gtk+-2.0 >= 2.6.0 \
             gthread-2.0 \
             lxpanel"

PKG_CHECK_MODULES(PACKAGE, [$pkg_modules])
AC_SUBST(PACKAGE_CFLAGS)
AC_SUBST(PACKAGE_LIBS)

prefix=$(pkg-config --variable=prefix lxpanel)
LXPANEL_LIBDIR=$(pkg-config --variable=libdir lxpanel)/lxpanel
LXPANEL_DATADIR=$(pkg-config --variable=datadir lxpanel)/lxpanel
AC_SUBST(LXPANEL_LIBDIR)
AC_SUBST(LXPANEL_DATADIR)

GETTEXT_PACKAGE=$PACKAGE
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE", [Gettext package.])

LXPANEL_MODULE="-avoid-version \
-no-undefined "'-export-symbols-regex "^[[^_]].*"'
AC_SUBST(LXPANEL_MODULE)

ALL_LINGUAS=""
AM_GLIB_GNU_GETTEXT

AC_OUTPUT([
  Makefile
  src/Makefile
  po/Makefile.in
])

echo
echo LXPanel Plugin: $PACKAGE
echo        Version: $VERSION
echo         Prefix: $prefix

