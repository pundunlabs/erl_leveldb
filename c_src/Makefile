all: 	leveldb_nif.cpp leveldb_lib.cpp
	g++ -Wall -g -o leveldb_nif.so -fpic -shared leveldb_nif.cpp leveldb_lib.cpp -L../../leveldb -lleveldb -lpthread -I ../../leveldb/include -I /opt/MA/otp/17.4/lib/erlang/usr/include/

clean:
	rm *.so
