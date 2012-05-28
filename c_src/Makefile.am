bin_PROGRAMS = erld

erld_SOURCES = cnode.c debug.c erld.c log.c main.c pid.c slay.c util.c

erld_CFLAGS=-I$(ERLANG_LIB_DIR_erl_interface)/include -D_REENTRANT
erld_LDADD=-L$(ERLANG_LIB_DIR_erl_interface)/lib -lei -lerl_interface -lpthread -lproc
