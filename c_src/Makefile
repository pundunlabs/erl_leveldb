PRIV_DIR:=../priv
override CC=g++
RM ?= rm -f

<<<<<<< Updated upstream
CPP_FILES = \
   leveldb_nif.cpp \
   leveldb_lib.cpp

HEADER_FILES = \
    leveldb_nif.h

UTILS_SOURCES=\
        leveldb_utils.cpp

override CFLAGS += -std=c++11 -Wall -g -O3 -fPIC $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')

ifeq ($(shell uname -s), Darwin)
override LDFLAGS +=  -bundle -flat_namespace -undefined suppress -fpic  -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
=======
SOURCES=\
        leveldb_nif.cpp \
        leveldb_lib.cpp

UTILS_SOURCES=\
        leveldb_utils.cpp

#all: 	leveldb_nif.cpp leveldb_lib.cppddd
#	g++ -Wall -g -o leveldb_nif.so -fpic -shared leveldb_nif.cpp leveldb_lib.cpp -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include -I /opt/MA/otp/17.4/lib/erlang/usr/include/

override CFLAGS += -std=c++11 -Wall -g -O3 -fPIC $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')

ifeq ($(shell uname -s), Darwin)
override LDFLAGS +=  -bundle -flat_namespace -undefined suppress -fpic leveldb_nif.cpp -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
>>>>>>> Stashed changes
else
override LDFLAGS += -shared -fpic -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
endif

all: $(PRIV_DIR)/leveldb_nif.so \
    $(PRIV_DIR)/leveldb_utils.so
<<<<<<< Updated upstream

$(PRIV_DIR)/leveldb_nif.so: $(CPP_FILES) $(HEADER_FILES)
	mkdir -p $(PRIV_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) $(CPP_FILES) -o $@
=======
>>>>>>> Stashed changes

$(PRIV_DIR)/leveldb_utils.so:
	mkdir -p $(PRIV_DIR)
<<<<<<< Updated upstream
	$(CC) $(CFLAGS) $(LDFLAGS) $(UTILS_SOURCES) -o $@
=======
	$(CC) $(CFLAGS) $(SOURCES) $(LDFLAGS) -o $@

$(PRIV_DIR)/leveldb_utils.so:
	mkdir -p $(PRIV_DIR)
	$(CC) $(CFLAGS) $(UTILS_SOURCES) $(LDFLAGS) -o $@
>>>>>>> Stashed changes

clean:
	$(RM) $(PRIV_DIR)/leveldb_nif.so \
	$(RM) $(PRIV_DIR)/leveldb_utils.so
