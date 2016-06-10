/* leveldb_nif.h */
#ifndef LEVELDB_NIF_H
#define LEVELDB_NIF_H

#include <mutex>          // std::mutex
#include <unordered_set>  //std::unordered_set
#include "leveldb/db.h"
#include "leveldb/comparator.h"
#include "descendingcomparator.h"
#include "leveldb/write_batch.h"
#include "erl_nif.h"

#define MAXPATHLEN       255

using namespace std;

typedef struct _opt_obj_resource {
  void *object;
} opt_obj_resource;

typedef struct _db_obj_resource {
  char allocated;
  unordered_set<void*> *link_set;
  mutex *mtx;
  void *object;
} db_obj_resource;

typedef struct _it_obj_resource {
  char allocated;
  mutex *mtx;
  void *linked_obj;
  void *object;
} it_obj_resource;

extern void delete_db(db_obj_resource* rdb);

extern void delete_rit(it_obj_resource* rit);

extern leveldb::DB* open_db(leveldb::Options* options, char* path, leveldb::Status* status);


extern int init_options(ErlNifEnv* env, const ERL_NIF_TERM* options_array, leveldb::Options **options);

extern int init_readoptions(ErlNifEnv* env, const ERL_NIF_TERM* readoptions_array, leveldb::ReadOptions **readoptions);

extern int init_writeoptions(ErlNifEnv* env, const ERL_NIF_TERM* writeoptions_array, leveldb::WriteOptions **writeoptions);

extern void init_db(leveldb::DB* db);

extern ERL_NIF_TERM make_status_tuple(ErlNifEnv* env, leveldb::Status status);
#endif /*LEVELDB_NIF_H*/
