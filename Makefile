include ./vsn.mk
VSN=$(LEVELDB_VSN)
APP_NAME = leveldb

SUBDIRS = c_src src test

.PHONY: all subdirs $(SUBDIRS) edoc eunit clean

all: edoc subdirs eunit

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

edoc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}, {source_path, ["src", "test"]}]'

eunit:
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

clean:
	rm -f ./ebin/*
	rm -f ./c_src/leveldb_nif.so*

realclean: clean
	 rm -rf ./deps/*
