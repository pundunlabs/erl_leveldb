#!/bin/sh
mkdir -p deps
cd deps
if [ ! -d "leveldb" ]; then
  	# Control will enter here if leveldb doesn't exist.
  	#git clone https://github.com/google/leveldb.git
	git clone http://skelter/gits/leveldb.git
fi
cd leveldb

git checkout v1.18

if [ -f "libleveldb.a" ]
then
	echo "libleveldb.a found."
else
	echo "libleveldb.a not found."
	make
fi

cd ../..
make all

