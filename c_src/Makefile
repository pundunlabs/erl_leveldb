PRIV_DIR:=../priv
override CC=g++
RM ?= rm -f

#all: 	leveldb_nif.cpp leveldb_lib.cppddd
#	g++ -Wall -g -o leveldb_nif.so -fpic -shared leveldb_nif.cpp leveldb_lib.cpp -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include -I /opt/MA/otp/17.4/lib/erlang/usr/include/

override CFLAGS += -Wall -g -O3 -fPIC $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')

ifeq ($(shell uname -s), Darwin)
override LDFLAGS +=  -bundle -flat_namespace -undefined suppress -fpic leveldb_nif.cpp leveldb_lib.cpp -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
else
override LDFLAGS += -shared -fpic leveldb_nif.cpp leveldb_lib.cpp -L../deps/leveldb -lleveldb -lpthread -I ../deps/leveldb/include
endif

all: $(PRIV_DIR)/leveldb_nif.so

$(PRIV_DIR)/leveldb_nif.so:
	mkdir -p $(PRIV_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@

clean:
	$(RM) $(PRIV_DIR)/leveldb_nif.so
