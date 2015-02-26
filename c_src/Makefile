PRIV_DIR:=../priv
override CC=g++
RM ?= rm -f

CPP_FILES = \
   leveldb_nif.cpp \
   leveldb_lib.cpp

HEADER_FILES = \
    leveldb_nif.h

override CFLAGS += -Wall -g -O3 -fPIC $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')

ifeq ($(shell uname -s), Darwin)
override LDFLAGS +=  -bundle -flat_namespace -undefined suppress -fpic  -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
else
override LDFLAGS += -shared -fpic -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
endif

all: $(PRIV_DIR)/leveldb_nif.so

$(PRIV_DIR)/leveldb_nif.so: $(CPP_FILES) $(HEADER_FILES)
	mkdir -p $(PRIV_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) $(CPP_FILES) -o $@

clean:
	$(RM) $(PRIV_DIR)/leveldb_nif.so
