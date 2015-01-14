-module(leveldb).

-export([open_db/2,
	 close_db/1,
	 get/3,
	 put/4,
	 delete/3,
	 write/4]).

-export([options/1,
	 readoptions/1,
	 writeoptions/1]).

-export([resource_test/0]).
	 
-on_load(init/0).

-include("leveldb.hrl").

init() ->
    ok = erlang:load_nif("/home/erdem/erl_leveldb/c_src/leveldb_nif", 0).

%% leveldb operations
open_db(_options, _Path)->
    erlang:nif_error(nif_library_not_loaded).

close_db(_db)->
    erlang:nif_error(nif_library_not_loaded).

get(_db, _readoptions, _Key)->
    erlang:nif_error(nif_library_not_loaded).

put(_db, _writeoptions, _Key, _Value)->
    erlang:nif_error(nif_library_not_loaded).

delete(_db, _writeoptions, _Key)->
    erlang:nif_error(nif_library_not_loaded).
    
write(_db, _writeoptions, _Delete_Ks, _Put_KVs)->
    erlang:nif_error(nif_library_not_loaded).

%% Declaring structs and constructing objects
options(_leveldb_options_record) ->
    erlang:nif_error(nif_library_not_loaded).

readoptions(_leveldb_readoptions_record) ->
    erlang:nif_error(nif_library_not_loaded).

writeoptions(_leveldb_writeoptions_record) ->
    erlang:nif_error(nif_library_not_loaded).

%%NIF test to allocate resources
resource_test()->
    erlang:nif_error(nif_library_not_loaded).
