PRIV_DIR:=../priv
override CC=g++
RM ?= rm -f

CPP_FILES = \
   leveldb_nif.cpp \
   leveldb_lib.cpp

HEADER_FILES = \
    leveldb_nif.h

UTILS_HEADERS = \
    keyvaluepair.h \


UTILS_SOURCES=\
    leveldb_utils.cpp

#override CFLAGS += -std=c++11 -Wall -g -O3 -fPIC $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')
override CFLAGS += -Wall -g -O3 -fPIC $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')

ifeq ($(shell uname -s), Darwin)
override LDFLAGS +=  -bundle -flat_namespace -undefined suppress -fpic  -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
else
override LDFLAGS += -shared -fpic -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
endif

all: $(PRIV_DIR) $(PRIV_DIR)/leveldb_nif.so $(PRIV_DIR)/leveldb_utils.so 

$(PRIV_DIR):
	mkdir -p $@

$(PRIV_DIR)/leveldb_nif.so: $(CPP_FILES) $(HEADER_FILES)
	$(CC) $(CFLAGS) $(CPP_FILES) $(LDFLAGS) -o $@

$(PRIV_DIR)/leveldb_utils.so: $(UTILS_SOURCES) $(UTILS_HEADERS)
	$(CC) $(CFLAGS) $(UTILS_SOURCES) $(LDFLAGS) -o $@

clean:
	$(RM) $(PRIV_DIR)/leveldb_nif.so
	$(RM) $(PRIV_DIR)/leveldb_utils.so
