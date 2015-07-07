/* leveldb_nif.h */
#ifndef LEVELDB_NIF_H
#define LEVELDB_NIF_H

#include "leveldb/db.h"
#include "leveldb/comparator.h"
#include "descendingcomparator.h"
#include "leveldb/write_batch.h"
#include "erl_nif.h"

#define MAXPATHLEN       255

extern leveldb::DB* open_db(leveldb::Options* options, char* path, leveldb::Status* status);

extern void close_db(leveldb::DB* db);

extern int init_options(ErlNifEnv* env, const ERL_NIF_TERM* options_array, leveldb::Options **options);

extern int init_readoptions(ErlNifEnv* env, const ERL_NIF_TERM* readoptions_array, leveldb::ReadOptions **readoptions);

extern int init_writeoptions(ErlNifEnv* env, const ERL_NIF_TERM* writeoptions_array, leveldb::WriteOptions **writeoptions);

extern void init_db(leveldb::DB* db);

extern ERL_NIF_TERM make_status_tuple(ErlNifEnv* env, leveldb::Status status);
#endif /*LEVELDB_NIF_H*/
