#!/bin/sh
LEVELDB_DIR=c_src/leveldb
LEVELDB_TAG=v1.18

if [ ! -d ${LEVELDB_DIR} ]; then
  	# Control will enter here if leveldb doesn't exist.
  	#git clone https://github.com/google/leveldb.git
	(cd c_src && git clone https://github.com/pundunlabs/leveldb.git && cd leveldb && git checkout $LEVELDB_TAG)
fi

if [ -f "${LEVELDB_DIR}/libleveldb.a" ]
then
	echo "libleveldb.a found."
else
	echo "libleveldb.a not found."
	export INSTALL_PATH="."
	(cd ${LEVELDB_DIR} && make)
fi

