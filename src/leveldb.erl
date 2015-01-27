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

-export_type([db/0,
              options/0,
	      writeoptions/0,
	      readoptions/0]).	 

-on_load(init/0).

-include("leveldb.hrl").

-opaque db() :: binary().
-opaque options() :: binary().
-opaque writeoptions() :: binary().
-opaque readoptions() :: binary().

-type value() :: binary() | string().
-type key() :: binary() | string().


init() ->
    ok = erlang:load_nif("/home/erdem/erl_leveldb/c_src/leveldb_nif", 0).

%% leveldb operations

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Open a leveldb database with provided Options on provided Path.
%% Returns {ok, DB} where DB is an NIF Resource, or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec open_db(Options :: options(), Path :: string()) -> {ok, DB :: db()} | {error, Reason :: any()}.
open_db(_options, _Path)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Close a leveldb database that is referenced by provided NIF Resource, DB.
%% @end
%%--------------------------------------------------------------------
-spec close_db(DB :: db()) -> ok | {error, Reason :: any()}.
close_db(_db)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Get the value of the key provided by Key from leveldb database referenced by NIF Resource DB.
%% Operation performed using provided ReadOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec get(DB :: db(), ReadOptions :: readoptions(), Key :: key()) -> {ok, value()} | {error, Reason :: any()}.
get(_db, _readoptions, _Key)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Put key/value pair provided by Key and Value to leveldb database referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec put(DB :: db(), WriteOptions :: writeoptions(), Key :: key(), Value :: value()) -> ok | {error, Reason :: any()}.
put(_db, _writeoptions, _Key, _Value)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Delete the key/value pair refered by the provided Key from leveldb database referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec delete(DB :: db(), WriteOptions :: writeoptions(), Key :: key()) -> ok | {error, Reason :: any()}.
delete(_db, _writeoptions, _Key)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Write a batch of keys to delete and key/value pairs to put provided by DeleteKeys and PutKeyValuePairs to leveldb database referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec write(DB :: db(), WriteOptions :: writeoptions(), DeleteKeys :: [key()], PutKeyValuePairs :: [{key(), value()}]) -> ok | {error, Reason :: any()}.
write(_db, _writeoptions, _Delete_Ks, _Put_KVs)->
    erlang:nif_error(nif_library_not_loaded).

%% Declaring structs and constructing objects

%%--------------------------------------------------------------------
%% @doc Make and get a NIF resource constructed by the provided LeveldbOptions.
%% This resource can be used for first arity in open_db/2 function.
%% @end
%%--------------------------------------------------------------------
-spec options(LeveldbOptions :: #leveldb_options{}) -> {ok, options()} | {error, Reason :: any()}.
options(_leveldb_options_record) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Make and get a NIF resource constructed by the provided LeveldbReadOptions.
%% This resource can be used for second arity in get/3 function.
%% @end
%%--------------------------------------------------------------------
-spec readoptions(LeveldbReadOptions :: #leveldb_readoptions{}) -> {ok, readoptions()} | {error, Reason :: any()}.
readoptions(_leveldb_readoptions_record) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Make and get a NIF resource constructed by the provided LeveldbWriteOptions.
%% This resource can be used for second arity in put/4. delete/3, write/4 functions.
%% @end
%%--------------------------------------------------------------------
-spec writeoptions(LeveldbWriteOptions :: #leveldb_writeoptions{}) -> {ok, writeoptions()} | {error, Reason :: any()}.
writeoptions(_leveldb_writeoptions_record) ->
    erlang:nif_error(nif_library_not_loaded).

%%NIF test to allocate resources

%%--------------------------------------------------------------------
%% @doc This function is used by developers for simple functionality tests and will be removed.
%% @end
%%--------------------------------------------------------------------
-spec resource_test() -> any().
resource_test()->
    erlang:nif_error(nif_library_not_loaded).
