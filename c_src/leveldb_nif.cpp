
#include "leveldb_nif.h"
#include <string>

#include <iostream>
#include <vector>

using namespace std;

static ErlNifResourceFlags resource_flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

static ErlNifResourceType* myOptionsResource;
static ErlNifResourceType* myDBResource;


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    myDBResource = enif_open_resource_type(env,
					   "leveldb_nif", 
					   "mydb_resource",
					   NULL,
					   resource_flags,
					   NULL);

    myOptionsResource = enif_open_resource_type(env,
						"leveldb_nif", 
						"options_resource",
						NULL,
						resource_flags,
						0);
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data,  void** old_priv_data,ERL_NIF_TERM load_info){
    return 0;
}

/*Test NIFs for experimenting*/
static ERL_NIF_TERM resource_test_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term;
    /* ERL_NIF_TERM status_term; */
    /*return  enif_make_atom(env, "ok");*/
    if(!myDBResource){
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "resource_type"));
    }

    void* db_ptr = enif_alloc_resource(myDBResource, 1024);

    // //leveldb::DB* db;
    // leveldb::Options options; 
    // options.create_if_missing = true;
    // leveldb::Status status = leveldb::DB::Open(options, "/tmp/testdb", &db);
    term = enif_make_resource(env, db_ptr);

    enif_release_resource(db_ptr);
    return term;
}

/*leveldb operations*/
static ERL_NIF_TERM open_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    leveldb::Options* options;

    /*get options resource*/
    if (argc != 2 || !enif_get_resource(env, argv[0], myOptionsResource, (void **) &options)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "options"));
	    //Return enif_make_badarg(env);
    }
    /*get path*/
    else if(enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) <1){
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "path"));
	    //return enif_make_badarg(env);
    }
    else{
        ERL_NIF_TERM db_term;
	    /* ERL_NIF_TERM status_term; */

	    leveldb::DB** db_ptr = (leveldb::DB**)  enif_alloc_resource(myDBResource, sizeof( leveldb::DB*));

        leveldb::Status status;

	    *db_ptr = open_db(options, path, &status);

	    if(status.ok()){
	        db_term = enif_make_resource(env, db_ptr);
	        enif_release_resource(db_ptr);
	        /* resource now only owned by "Erlang" */
            return enif_make_tuple2(env, enif_make_atom(env, "ok"), db_term);
	    }
	    else{
	        ERL_NIF_TERM status_tuple = make_status_tuple(env, status);
	        return status_tuple;
	    }
    }
}

static ERL_NIF_TERM close_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    leveldb::DB** db_ptr;

    /*get db_ptr resource*/
    if (argc != 1 || !enif_get_resource(env, argv[0], myDBResource, (void **) &db_ptr)) {
	    return enif_make_badarg(env);
    }
    else{
	    close_db(db_ptr);
	    return enif_make_atom(env, "ok");
    }
}

static ERL_NIF_TERM get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    leveldb::DB** db_ptr;
    leveldb::ReadOptions* readoptions;
    ErlNifBinary binkey;
    std::string value;
    ErlNifBinary binvalue;
    ERL_NIF_TERM value_term;

    /*get db_ptr resource*/
    if (argc != 3 || !enif_get_resource(env, argv[0], myDBResource, (void **) &db_ptr)) {
	    return enif_make_badarg(env);
    }

    /*get readoptions resource*/
    else if (!enif_get_resource(env, argv[1], myOptionsResource, (void **) &readoptions)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "readoptions"));
	    //Return enif_make_badarg(env);
    }

    /*get key resource*/
    else if (!enif_inspect_binary(env, argv[2], &binkey)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "key"));
	    //Return enif_make_badarg(env);
    }

    else{
	    leveldb::Slice key((const char*)binkey.data, (size_t) binkey.size);
	    leveldb::Status status = (*db_ptr)->Get(*readoptions, key, &value);

	    if(status.ok()){
	        //cout << key.ToString() << "->" << value << endl;
	        enif_alloc_binary(value.length(), &binvalue);
	        memcpy(binvalue.data, value.data(), value.length());
	        //binvalue.data = (unsigned char*) value.c_str();
	        value_term = enif_make_binary(env, &binvalue);
	        /*not calling enif_release_binary since enif_make_binary transferrs ownership*/
            return enif_make_tuple2(env, enif_make_atom(env, "ok"), value_term);
	    }
	    else{
	        ERL_NIF_TERM status_tuple = make_status_tuple(env, status);
	        return status_tuple;
	    }
    }
}

static ERL_NIF_TERM put_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    leveldb::DB** db_ptr;
    leveldb::WriteOptions* writeoptions;
    ErlNifBinary binkey;
    ErlNifBinary binvalue;

    /*get db_ptr resource*/
    if (argc != 4 || !enif_get_resource(env, argv[0], myDBResource, (void **) &db_ptr)) {
	    return enif_make_badarg(env);
    }
    
    /*get writeoptions resource*/
    else if (!enif_get_resource(env, argv[1], myOptionsResource, (void **) &writeoptions)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "writeoptions"));
	    //Return enif_make_badarg(env);
    }

    /*get key resource*/
    else if (!enif_inspect_binary(env, argv[2], &binkey)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "key"));
	    //Return enif_make_badarg(env);
    }
    
    /*get value resource*/
    else if (!enif_inspect_binary(env, argv[3], &binvalue)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "value"));
	    //Return enif_make_badarg(env);
    }
    
    else{
	    leveldb::Slice key((const char*)binkey.data, (size_t) binkey.size);
	    leveldb::Slice value((const char*)binvalue.data, (size_t) binvalue.size);

	    leveldb::Status status = (*db_ptr)->Put(*writeoptions, key, value);

	    if(status.ok()){
	        return enif_make_atom(env, "ok");
	    }
	    else{
	        ERL_NIF_TERM status_tuple = make_status_tuple(env, status);
	        return status_tuple;
	    }
    }
}

static ERL_NIF_TERM delete_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    leveldb::DB** db_ptr;
    leveldb::WriteOptions* writeoptions;
    ErlNifBinary binkey;

    /*get db_ptr resource*/
    if (argc != 3 || !enif_get_resource(env, argv[0], myDBResource, (void **) &db_ptr)) {
	    return enif_make_badarg(env);
    }

    /*get writeoptions resource*/
    else if (!enif_get_resource(env, argv[1], myOptionsResource, (void **) &writeoptions)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "writeoptions"));
	    //Return enif_make_badarg(env);
    }

    /*get key resource*/
    else if (!enif_inspect_binary(env, argv[2], &binkey)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "key"));
	    //Return enif_make_badarg(env);
    }

    else{
	    leveldb::Slice key((const char*)binkey.data, (size_t) binkey.size);
	    leveldb::Status status = (*db_ptr)->Delete(*writeoptions, key);

	    if(status.ok()){
	        return enif_make_atom(env, "ok");
	    }
	    else{
	        ERL_NIF_TERM status_tuple = make_status_tuple(env, status);
	        return status_tuple;
	    }
    }
}

static ERL_NIF_TERM write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    leveldb::DB** db_ptr;
    leveldb::WriteOptions* writeoptions;
    unsigned int delete_keys_size;
    unsigned int put_kvs_size;
    
    ERL_NIF_TERM head, tail;
    ErlNifBinary bin;
    ERL_NIF_TERM delete_list = argv[2];
    ERL_NIF_TERM put_list = argv[3];

    /*get db_ptr resource*/
    if (argc != 4 || !enif_get_resource(env, argv[0], myDBResource, (void **) &db_ptr)) {
	    return enif_make_badarg(env);
    }
    /*get writeoptions resource*/
    else if (!enif_get_resource(env, argv[1], myOptionsResource, (void **) &writeoptions)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "writeoptions"));
	    //Return enif_make_badarg(env);
    }
    /*get delete keys resource*/
    else if (!enif_get_list_length(env, delete_list, &delete_keys_size)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "delete_ks"));
	    //Return enif_make_badarg(env);
    }
    /*get put key/values resource*/
    else if (!enif_get_list_length(env, put_list, &put_kvs_size)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "put_kvs"));
	    //Return enif_make_badarg(env);
    }
    else{
	    //leveldb::Slice *delete_keys = new leveldb::Slice[delete_keys_size];
	    //leveldb::Slice *put_keys    = new leveldb::Slice[put_kvs_size];
	    //leveldb::Slice *put_values  = new leveldb::Slice[put_kvs_size];
        vector<leveldb::Slice> delete_keys;
        vector<leveldb::Slice> put_keys;
        vector<leveldb::Slice> put_values;

	
	    while(enif_get_list_cell(env, delete_list, &head, &tail)) {
	        if(!enif_inspect_binary(env, head, &bin)) {
		        return enif_make_badarg(env);
	        }
	        leveldb::Slice key((const char*)bin.data, (size_t) bin.size);
	        delete_keys.push_back(key);
	        delete_list = tail;
	    }

	    int arity;
	    const ERL_NIF_TERM* put_kv_array;
	    while(enif_get_list_cell(env, put_list, &head, &tail)) {
	        if(!enif_get_tuple(env, head, &arity, &put_kv_array)) {
		        return enif_make_badarg(env);
	        }
	        if(arity != 2 || !enif_inspect_binary(env, put_kv_array[0], &bin)) {
		        return enif_make_badarg(env);
	        }
	        leveldb::Slice key((const char*)bin.data, (size_t) bin.size);
	        put_keys.push_back(key);
	        if(!enif_inspect_binary(env, put_kv_array[1], &bin)) {
		        return enif_make_badarg(env);
	        }
	        leveldb::Slice value((const char*)bin.data, (size_t) bin.size);
	        put_values.push_back(value);
	        put_list = tail;
	    }
	
	    leveldb::WriteBatch batch;
	
	    while (!delete_keys.empty()){
	        batch.Delete(delete_keys.back());
            delete_keys.pop_back();
	    }

	    while (!put_keys.empty()){
	        batch.Put(put_keys.back(), put_values.back());
            put_keys.pop_back();
            put_values.pop_back();
	    }

	    leveldb::Status status = (*db_ptr)->Write(*writeoptions, &batch);

	    if(status.ok()){
	        return enif_make_atom(env, "ok");
	    }
	    else{
	        ERL_NIF_TERM status_tuple = make_status_tuple(env, status);
	        return status_tuple;
	    }
    }
}

/*Resource making*/
static ERL_NIF_TERM options_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM term;
    int arity;
    const ERL_NIF_TERM* options_array;
    int result;

    if (argc != 1 || !enif_get_tuple(env, argv[0], &arity, &options_array)) {
        return enif_make_badarg(env);
    }
    else{
	    leveldb::Options* options = ( leveldb::Options* ) enif_alloc_resource(myOptionsResource, sizeof( leveldb::Options));
	
	    result = init_options(env, options_array, options);
	    /*if result is 0 then return {ok, term}*/
	    if (!result){
	        term = enif_make_resource(env, options);
	    
	        enif_release_resource(options);
	        return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
	    }
	    else {
	        return enif_make_badarg(env);
	    }
    }
}

static ERL_NIF_TERM readoptions_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term;
    int arity;
    const ERL_NIF_TERM* readoptions_array;
    int result;

    if (argc != 1 || !enif_get_tuple(env, argv[0], &arity, &readoptions_array)) {
        return enif_make_badarg(env);
    }
    else{
	    leveldb::ReadOptions* readoptions = ( leveldb::ReadOptions* ) enif_alloc_resource(myOptionsResource, sizeof( leveldb::ReadOptions));
	
	    result = init_readoptions(env, readoptions_array, readoptions);
	    /*if result is 0 then return {ok, term}*/
	    if (!result){
	        term = enif_make_resource(env, readoptions);
	    
	        enif_release_resource(readoptions);
	        return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
	    }
	    else {
	        return enif_make_badarg(env);
	    }
    }
}

static ERL_NIF_TERM writeoptions_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term;
    int arity;
    const ERL_NIF_TERM* writeoptions_array;
    int result;

    if (argc != 1 || !enif_get_tuple(env, argv[0], &arity, &writeoptions_array)) {
        return enif_make_badarg(env);
    }
    else{
	    leveldb::WriteOptions* writeoptions = ( leveldb::WriteOptions* ) enif_alloc_resource(myOptionsResource, sizeof( leveldb::WriteOptions));
	
	    result = init_writeoptions(env, writeoptions_array, writeoptions);

	    /*if result is 0 then return {ok, term}*/
        if (!result){
	        term = enif_make_resource(env, writeoptions);
	    
	        enif_release_resource(writeoptions);
	        return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
	    }
	    else {
	        return enif_make_badarg(env);
	    }
    }
}

/*leveldb destroy*/
static ERL_NIF_TERM destroy_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    leveldb::Options* options;
    
    /*get path*/
    if(argc != 2 || enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) <1){
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "path"));
	    //return enif_make_badarg(env);
    }
    /*get options resource*/
    else if (!enif_get_resource(env, argv[1], myOptionsResource, (void **) &options)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "options"));
	    //Return enif_make_badarg(env);
    }
    else{
	    leveldb::Status status = DestroyDB(path, *options);

	    if(status.ok()){
	        return enif_make_atom(env, "ok");
	    }
	    else{
	        ERL_NIF_TERM status_tuple = make_status_tuple(env, status);
	        return status_tuple;
	    }
    }
}

/*leveldb repair*/
static ERL_NIF_TERM repair_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    leveldb::Options* options;
    
    /*get path*/
    if(argc != 2 || enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) <1){
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "path"));
	    //return enif_make_badarg(env);
    }
    /*get options resource*/
    else if (!enif_get_resource(env, argv[1], myOptionsResource, (void **) &options)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "options"));
	    //Return enif_make_badarg(env);
    }
    else{
	    leveldb::Status status = RepairDB(path, *options);

	    if(status.ok()){
	        return enif_make_atom(env, "ok");
	    }
	    else{
	        ERL_NIF_TERM status_tuple = make_status_tuple(env, status);
	        return status_tuple;
	    }
    }
}

static ERL_NIF_TERM approximate_sizes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    leveldb::DB** db_ptr;
    unsigned int ranges_size;

    ERL_NIF_TERM head, tail;
    ErlNifBinary bin;

    ERL_NIF_TERM range_list = argv[1];

    /*get db_ptr resource*/
    if (argc != 2 || !enif_get_resource(env, argv[0], myDBResource, (void **) &db_ptr)) {
	return enif_make_badarg(env);
    }
    /*get ranges resource*/
    else if (!enif_get_list_length(env, range_list, &ranges_size)) {
	return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "ranges"));
	//Return enif_make_badarg(env);
    }
    else{
	vector <leveldb::Range>  ranges;    
	ranges.reserve( ranges_size );

	int arity;
	const ERL_NIF_TERM* range_array;
	while(enif_get_list_cell(env, range_list, &head, &tail)) {
	    if(!enif_get_tuple(env, head, &arity, &range_array)) {
		return enif_make_badarg(env);
	    }
	    if(arity != 2 || !enif_inspect_binary(env, range_array[0], &bin)) {
		return enif_make_badarg(env);
	    }
	    leveldb::Slice start((const char*)bin.data, (size_t) bin.size);
	    if(!enif_inspect_binary(env, range_array[1], &bin)) {
		return enif_make_badarg(env);
	    }
	    leveldb::Slice limit((const char*)bin.data, (size_t) bin.size);
	    ranges.push_back( leveldb::Range(start, limit) );
	    range_list = tail;
	}

	uint64_t sizes[ranges_size];

	(*db_ptr)->GetApproximateSizes(&ranges[0], ranges_size, sizes);

        ERL_NIF_TERM size_list = enif_make_list(env, 0);
        while (ranges_size > 0) {
            ranges_size--;
            size_list = enif_make_list_cell(env, enif_make_uint64(env, sizes[ranges_size]), size_list);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), size_list);
    }
}

static ERL_NIF_TERM read_range_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    leveldb::DB** db_ptr;
    leveldb::ReadOptions* readoptions;

    int arity;
    const ERL_NIF_TERM* range_array;
    ErlNifBinary bin;

    int max_keys;

    /*get db_ptr resource*/
    if (argc != 4 || !enif_get_resource(env, argv[0], myDBResource, (void **) &db_ptr)) {
	    return enif_make_badarg(env);
    }

    /*get readoptions resource*/
    else if (!enif_get_resource(env, argv[1], myOptionsResource, (void **) &readoptions)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "readoptions"));
	    //Return enif_make_badarg(env);
    }

    /*get range tuple*/
    else if (!enif_get_tuple(env, argv[2], &arity, &range_array)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "range"));
	    //Return enif_make_badarg(env);
    }

    /*get limit integer*/
    if (!enif_get_int(env, argv[3], &max_keys)) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "limit"));
    }

    else{
        /*Read range tuple into start and limit slices*/
        if(arity != 2 || !enif_inspect_binary(env, range_array[0], &bin)) {
            return enif_make_badarg(env);
        }
        leveldb::Slice start((const char*)bin.data, (size_t) bin.size);

        if(!enif_inspect_binary(env, range_array[1], &bin)) {
            return enif_make_badarg(env);
        }
        leveldb::Slice limit((const char*)bin.data, (size_t) bin.size);

        /*Create leveldb iterator*/
        leveldb::Iterator* it = (*db_ptr)->NewIterator(*readoptions);

        /*Create empty list to store key/value pairs*/
        //ERL_NIF_TERM kvl = enif_make_list(env, 0);
        ERL_NIF_TERM kvl;

        /*Declare key and value erlang resources*/
        ErlNifBinary binkey;
        ErlNifBinary binvalue;
        ERL_NIF_TERM key_term;
        ERL_NIF_TERM value_term;

        /*declare vector to keep key/value pairs*/
        vector<ERL_NIF_TERM> kvl_vector;

        /*Iterate through start to limit*/
        int i = 0;
        for (it->Seek(start);
             it->Valid() && it->key().ToString() < limit.ToString() && i < max_keys;
             it->Next()) {

            /*Construct key_term*/
            enif_alloc_binary(it->key().size(), &binkey);
            memcpy(binkey.data, it->key().data(), it->key().size());
            key_term = enif_make_binary(env, &binkey);
            /*Construct value_term*/
            enif_alloc_binary(it->value().size(), &binvalue);
            memcpy(binvalue.data, it->value().data(), it->value().size());
            value_term = enif_make_binary(env, &binvalue);

            //kvl = enif_make_list_cell(env, enif_make_tuple2(env, key_term, value_term), kvl);
            kvl_vector.push_back(enif_make_tuple2(env, key_term, value_term));
            i++;
        }

        kvl = enif_make_list_from_array(env, &kvl_vector[0], kvl_vector.size());
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), kvl);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"open_db", 2, open_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_db", 1, close_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get", 3, get_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"put", 4, put_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"delete", 3, delete_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write", 4, write_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"options", 1, options_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"readoptions", 1, readoptions_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"writeoptions", 1, writeoptions_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},

    {"destroy_db", 2, destroy_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"repair_db", 2, repair_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"approximate_sizes", 2, approximate_sizes_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_range", 4, read_range_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"resource_test", 0, resource_test_nif}

};

ERL_NIF_INIT(leveldb, nif_funcs, &load, NULL, &upgrade, NULL)
