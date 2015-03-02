
#include <string>

#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>    // std::make_heap, std::pop_heap, std::push_heap, std::sort_heap

#include "erl_nif.h"
#include "keyvaluepair.h"

using namespace std;
using namespace leveldb;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data,  void** old_priv_data, ERL_NIF_TERM load_info){
    return 0;
}

static ERL_NIF_TERM merge_sorted_kvls_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    /*kvls: the pointer to list of key/value lists passed from erlang*/
    ERL_NIF_TERM kvls = argv[0];
    /*kvl: the resulting erlang nif term*/
    ERL_NIF_TERM kvl;
    unsigned int kvls_len, len;
    ERL_NIF_TERM head, tail, h, t;
    int arity;
    const ERL_NIF_TERM* tuple;
    ErlNifBinary keybin, valuebin;
    int total_kvps = 0;
    /*get list of lists*/
    if (argc != 1 || !enif_get_list_length(env, kvls, &kvls_len)) {
        return enif_make_badarg(env);
    }

    /*Declare an array of vectors of type kvp struct (Key/Value Pair)*/
    std::queue<KeyValuePair> kvlq_array[kvls_len];
    
    /*Declare a vector of type kvp struct (Key/Value Pair) to use as min heap*/
    vector<KeyValuePair> maxheap;
    maxheap.reserve(kvls_len);

    int i = 0;
    while(enif_get_list_cell(env, kvls, &head, &tail)){
        if(!enif_get_list_length(env, head, &len)) {
            return enif_make_badarg(env);
        }
        
        queue<KeyValuePair> kvpq;
        bool at_head = true;
        while(enif_get_list_cell(env, head, &h, &t)){
            if(!enif_get_tuple(env, h, &arity, &tuple)) {
                return enif_make_badarg(env);
            }
            if(arity != 2 || !enif_inspect_binary(env, tuple[0], &keybin)) {
                return enif_make_badarg(env);
            }
            if(!enif_inspect_binary(env, tuple[1], &valuebin)) {
                return enif_make_badarg(env);
            }
            
            cout << "read: "<< string((const char*)keybin.data, keybin.size) << ": "  << string((const char*)valuebin.data, valuebin.size);
            KeyValuePair kvp(i, (const char*)keybin.data, (size_t) keybin.size,
                             (const char*)valuebin.data, (size_t) valuebin.size);
            
            if (at_head) {
                maxheap.push_back(kvp);
                cout << " heap: "<< string(kvp.key(), kvp.key_size()) << ": "  << string(kvp.value(), kvp.value_size()) << endl;
                at_head = false;    
            }
            else {
            cout << " queue: "<< string((const char*)keybin.data, keybin.size) << ": "  << string((const char*)valuebin.data, valuebin.size) << endl;
                kvpq.push( kvp );
            }
            total_kvps++;
            head = t;
        }
        kvlq_array[i] = kvpq;
        i++;
        kvls = tail;
    }
   
    /*Make the vector containing first element of each list a heap*/
    KeyValuePair comp; 
    make_heap (maxheap.begin(), maxheap.end(), comp);
    
    /*Declare the vector that keeps Erlang NIF Term representations*/
    vector<ERL_NIF_TERM> merged_kvls;
    /*We already know how much space we need, so we reserve it initially*/
    merged_kvls.reserve(total_kvps);
    
    /*Declare key and value erlang resources*/
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*Use int tag to keep track of an heap elements original vector*/
    int tag;
    KeyValuePair kvp;
    while (!maxheap.empty()) {
        /*Get root elemenet of the heap and put into merged_kvls*/
        kvp = maxheap.front();
        cout << string(kvp.key(), kvp.key_size()) << ": "  << string(kvp.value(), kvp.value_size()) << endl;
        tag = kvp.tag();
        memcpy(keybin.data, kvp.key(), kvp.key_size());
        key_term = enif_make_binary(env, &keybin);
        memcpy(valuebin.data, kvp.value(), kvp.value_size());
        value_term = enif_make_binary(env, &valuebin);
        merged_kvls.push_back( enif_make_tuple2(env, key_term, value_term) );
        /*Pop root element of the heap*/
        pop_heap ( maxheap.begin(), maxheap.end(), comp ); maxheap.pop_back();
        /*Push new element from kvl list of tag if not empty*/
        if (!kvlq_array[tag].empty()) {
            maxheap.push_back( kvlq_array[tag].front() ); 
            push_heap ( maxheap.begin(), maxheap.end(), comp );
            kvlq_array[tag].pop ();
        }
    }
    kvl = enif_make_list_from_array(env, &merged_kvls[0], merged_kvls.size());
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), kvl);
}

static ErlNifFunc nif_funcs[] = {
    {"merge_sorted_kvls", 1, merge_sorted_kvls_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(leveldb_utils, nif_funcs, &load, &reload, &upgrade, NULL)

