
#include <string>

#include <iostream>
#include <vector>
#include <array>
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
    ERL_NIF_TERM kvls = argv[0];
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
    vector<KeyValuePair> kvl_array[kvls_len];
    
    /*Declare a vector of type kvp struct (Key/Value Pair) to use as min heap*/
    vector<KeyValuePair> minheap;
    minheap.reserve(kvls_len);

    int i = 0;
    while(enif_get_list_cell(env, kvls, &head, &tail)){
        if(!enif_get_list_length(env, head, &len)) {
            return enif_make_badarg(env);
        }
        
        vector<KeyValuePair> kvpv;
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
            
            KeyValuePair kvp(i, (const char*)keybin.data, (size_t) keybin.size,
                             (const char*)valuebin.data, (size_t) valuebin.size);
            
            kvpv.push_back( kvp );
            if (at_head) {
                minheap.push_back(kvp);
                at_head = false;    
            }
            else {
                kvpv.push_back( kvp );
            }
            total_kvps++;
            head = t;
        }
        kvl_array[i] = kvpv;
        i++;
        kvls = tail;
    }
   
    KeyValuePair comp; 
    make_heap (minheap.begin(), minheap.end(), comp);
    //make_heap (minheap.begin(),minheap.end());
    
    vector<ERL_NIF_TERM> merged_kvls;
    merged_kvls.reserve(total_kvps);
    
    /*Declare key and value erlang resources*/
    ErlNifBinary binkey;
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;
    
    int tag;
    KeyValuePair kvp;
    while (!minheap.empty()) {
        /*Get root elemenet of the heap and put into merged_kvls*/
        kvp = minheap.front();
        tag = kvp.tag();
        memcpy(binkey.data, kvp.data(), kvp.size());
        key_term = enif_make_binary(env, &binkey);
        memcpy(binvalue.data, kvp.value(), kvp.value_size());
        value_term = enif_make_binary(env, &binvalue);
        merged_kvls.push_back( enif_make_tuple2(env, key_term, value_term) );
        /*Pop root element of the heap*/
        pop_heap ( minheap.begin(), minheap.end(), comp ); minheap.pop_back();
        /*Push new element from kvl list of tag if not empty*/
        if (!kvl_array[tag].empty()) {
            minheap.push_back( kvl_array[tag].back() ); 
            push_heap ( minheap.begin(), minheap.end(), comp );
        }
    }
    kvl = enif_make_list_from_array(env, &merged_kvls[0], merged_kvls.size());
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), kvl);
}

static ErlNifFunc nif_funcs[] = {
    {"merge_sorted_kvls", 1, merge_sorted_kvls_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(leveldb, nif_funcs, &load, &reload, &upgrade, NULL)

