include ../vsn.mk
VSN=$(LEVELDB_VSN)

EBIN = ../ebin

RM ?= rm -f

APP_NAME = leveldb
APP_FILE= $(APP_NAME).app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

MODULES = \
	leveldb \
    leveldb_lib

ERL_FILES = $(MODULES:%=%.erl)

INCLUDE_DIRS = \
	../include

.PHONY: all clean

all: $(EBIN) $(APP_TARGET)
	erlc -I $(INCLUDE_DIRS) -o $(EBIN) $(ERL_FILES)

$(EBIN):
	mkdir ../ebin

$(APP_TARGET): $(APP_SRC) 
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	$(RM) ../ebin/*.app
	$(RM) ../ebin/*.beam
