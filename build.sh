#!/bin/sh
mkdir -p deps
leveldb_dir=deps/leveldb
LEVELDB_TAG=v1.18

if [ ! -d ${leveldb_dir} ]; then
  	# Control will enter here if leveldb doesn't exist.
  	#git clone https://github.com/google/leveldb.git
	(cd deps && git clone http://skelter/gits/leveldb.git &&
     cd leveldb && git checkout $LEVELDB_TAG)
fi

if [ -f "${leveldb_dir}/libleveldb.a" ]
then
	echo "libleveldb.a found."
else
	echo "libleveldb.a not found."
	(cd ${leveldb_dir} && make)
fi

