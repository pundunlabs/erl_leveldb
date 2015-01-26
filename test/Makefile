include ../vsn.mk
VSN=$(LEVELDB_VSN)

EBIN = ../ebin


MODULES = \
	phofs \
	leveldb_test

ERL_FILES = $(MODULES:%=%.erl)

INCLUDE_DIRS = \
	../include

.PHONY: all clean

all: $(EBIN)
	erlc -I $(INCLUDE_DIRS) -o $(EBIN) $(ERL_FILES)

$(EBIN):
	mkdir ../ebin

$(APP_TARGET): $(APP_SRC) 
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	rm ../ebin/*.app
	rm ../ebin/*.beam
