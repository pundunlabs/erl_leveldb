#include "leveldb_nif.h"
#include <assert.h>

#include <iostream>

using namespace std;
using namespace leveldb;

namespace {
  DescendingComparator descendingcomparator;

  int get_bool(ErlNifEnv* env, ERL_NIF_TERM term)
  {
      char buf[6];

      if(enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1)){
	  if (strcmp("false", buf) == 0)
	      return 0;
	  if (strcmp("true", buf) == 0)
	      return 1;
      }
      return -1;
  }
}

int init_options(ErlNifEnv* env, const ERL_NIF_TERM* options_array, leveldb::Options **_options) {
    int temp = -1;
    leveldb::Options *options = new leveldb::Options;
    *_options = options;

    /* 0. leveldb_options,
    // 1. comparator,
    // 2. create_if_missing,
    // 3. error_if_exists,
    // 4. paranoid_checks,
    // 5. env,
    // 6. info_log,
    // 7. write_buffer_size,
    // 8. max_open_files,
    // 9. block_cache,
    // 10. block_size,
    // 11. block_restart_interval,
    // 12. compression,
    // 13. filter_policy*/
    
    //Set comparator
    if (!enif_get_int(env, options_array[1], &temp)){
	return -1;
    }

    if ( temp == 0 ) {
	options->comparator = &descendingcomparator;
    }
     
    //Set create_if_missing
    temp = get_bool(env, options_array[2]);
    if (temp == -1) {
	return -1;
    }
    options->create_if_missing = temp;
    
    //Set error_if_exists
    temp = get_bool(env, options_array[3]);
    if (temp == -1) {
	return -1;
    }
    options->error_if_exists = temp;

    //Set write_buffer_size
    if (!enif_get_int(env, options_array[7], &temp)){
	return -1;
    }
    options->write_buffer_size = temp * 1048576; /*(1024 * 1024)(MB)*/
    
    //Set max_open_files
    if (!enif_get_int(env, options_array[8], &temp)){
	return -1;
    }
    options->max_open_files = temp;

    //Set block_size
    if (!enif_get_int(env, options_array[10], &temp)){
	return -1;
    }
    options->block_size = temp * 1024; /*(KB)*/
    
    //Set block_restart_interval
    if (!enif_get_int(env, options_array[11], &temp)){
	return -1;
    }
    options->block_restart_interval = temp;

    return 0;
}

int init_readoptions(ErlNifEnv* env, const ERL_NIF_TERM* readoptions_array, leveldb::ReadOptions **_readoptions) {
    int temp;
    leveldb::ReadOptions *readoptions = new leveldb::ReadOptions;
    *_readoptions = readoptions;

    /* 0. leveldb_options,
    // 1. verify_checksums,
    // 2. fill_cache,
    // 3. snapshot*/
    
    //Set verify_checksums
    temp = get_bool(env, readoptions_array[1]);
    if (temp == -1) {
	return -1;
    }
    else{
	readoptions->verify_checksums = temp;
    }
    
    //Set fill_cache
    temp = get_bool(env, readoptions_array[2]);
    if (temp == -1) {
	return -1;
    }
    else{
	readoptions->fill_cache = temp;
    }
    return 0;
}

int init_writeoptions(ErlNifEnv* env, const ERL_NIF_TERM* writeoptions_array, leveldb::WriteOptions **_writeoptions) {
    int temp;
    leveldb::WriteOptions *writeoptions = new leveldb::WriteOptions;
    *_writeoptions = writeoptions;
    
    /* 0. leveldb_writeoptions
    // 1. sync*/
    
    //Set sync
    temp = get_bool(env, writeoptions_array[1]);
    if (temp == -1) {
	return -1;
    }
    else{
	writeoptions->sync = temp;
    }
    return 0;
}

leveldb::DB* open_db(leveldb::Options* options, char* path, leveldb::Status* status) {
    leveldb::DB* db;
    string path_string(path);
    *status = leveldb::DB::Open(*options, path_string, &db);
    //Prints "Status: OK" if successful
    //cout << "Status: " << status->ToString() << "\n" << endl;
    //assert(status.ok());
    return db;
}

void close_db(leveldb::DB* db){
    delete db;
}

extern ERL_NIF_TERM make_status_tuple(ErlNifEnv* env, leveldb::Status status){
    const char* type;
    if(status.IsNotFound()){
	type = "not_found";
    }
    else if(status.IsCorruption()){
	type = "corruption";
    }
    else if(status.IsIOError()){
	type = "io_error";
    }
    else{
	type = "unspecified";
    }
    const char* stString = status.ToString().c_str();
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
			    enif_make_tuple2(env, enif_make_atom(env, type),
					     enif_make_string(env, stString, ERL_NIF_LATIN1)));
}
