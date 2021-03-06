include ./vsn.mk
VSN=$(ERL_LEVELDB_VSN)
APP_NAME = erl_leveldb
MAKE ?= make
SUBDIRS = c_src src test

.PHONY: all subdirs $(SUBDIRS) edoc eunit clean

all: script subdirs

script:
	./build.sh

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

edoc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}, {source_path, ["src", "test"]}]'

eunit:
	./ld_library_path \
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

clean:
	@for i in $(SUBDIRS); do \
	echo "Cleaning in $$i..."; \
	(cd $$i; $(MAKE) clean); done

realclean: clean
	 rm -rf ./deps/*
