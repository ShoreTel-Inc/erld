AC_INIT(UNIX daemon wrapper for Erlang, 1.0.1, bernie@m5.net, erld)
AC_PREREQ(2.59c)
AC_COPYRIGHT(Copyright (C) 2012 ShoreTel Inc.)


AM_INIT_AUTOMAKE

CFLAGS=" $CFLAGS -g -D__RELFILE__=__FILE__ -D_BSD_SOURCE"

LDFLAGS=" $LDFLAGS -g"

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AM_PROG_CC_C_O

# Checks for header files.
AC_STDC_HEADERS
AC_CHECK_HEADERS([proc/readproc.h])

AC_SEARCH_LIBS([readproctab], [proc procps], , AC_MSG_ERROR([Unable to find readproctab(). Make sure either libproc or libprocps is installed]))

AC_ERLANG_CHECK_LIB(erl_interface)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)
AC_SUBST([ERLCFLAGS], [$erlcflags])

# Checks for typedefs, structures, and compiler characteristics.
AC_CONFIG_FILES([ \
                 Makefile \
                 c_src/Makefile \
                 erld_erlang_app/Makefile \
                 example/Makefile \
                 ])

AC_OUTPUT
echo
echo $PACKAGE $VERSION
echo
eval eval echo erld will be installed in $bindir.
echo
echo configure complete, now type 'make' and then 'make install'
echo
