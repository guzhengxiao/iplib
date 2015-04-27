ERL=$(shell which erl)
REBAR=$(shell which rebar || echo ./rebar)
APP=iplib
PA=./ebin ./deps/*/ebin

.PHONY: deps

all: deps compile

compile:
	    @$(REBAR) compile
deps:
	    @$(REBAR) get-deps
clean:
	    @$(REBAR) clean
distclean: clean
	    @$(REBAR) delete-deps
generate:
	    @$(REBAR) generate
start: all
	exec erl -pa ebin deps/*/ebin -boot start_sasl \
	-name "iplib@127.0.0.1" \
	-config rel/files/sys.config \
	-s iplib \
	-sasl errlog_type error \
	-mnesia dir "\"data/mnesia\"" 
	-kernel error_logger '{file,"data/log/error.log"}' \
	-sasl sasl_error_logger '{file,"data/log/sasl_error.log"}' \
	-os_mon start_cpu_sup true \
	-os_mon start_disksup false \
	-os_mon start_memsup false \
	-env ERL_CRASH_DUMP "data/log/erlang_crash_$$.dump" \
	+K true \
	+P 65536
