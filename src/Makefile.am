erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_erld)/src
erlsrc_DATA = erld_app.erl erld.erl erld_heartbeat.erl erld_remote.erl
EXTRA_DIST = $(erlsrc_DATA) erld.app.src

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_erld)/ebin
erlbeam_DATA = erld_app.beam erld.beam erld_heartbeat.beam erld_remote.beam
CLEANFILES = $(erlbeam_DATA)

SUFFIXES = .erl .beam 
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
