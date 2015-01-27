include ./vsn.mk
VSN=$(LEVELDB_VSN)
APP_NAME = leveldb

SUBDIRS = c_src src test

.PHONY: all subdirs $(SUBDIRS) edoc clean

all: edoc subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

edoc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
               '"."' '[{def,{vsn,"$(VSN)"}}, {source_path, ["src", "test"]}]'

clean:
	rm ./ebin/*
	rm ./c_src/leveldb_nif.so*

realclean: clean
	 rm -rf ./deps/*
