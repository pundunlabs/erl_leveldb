%%%-------------------------------------------------------------------
%%% @author erdem aksu <erdem@sitting>
%%% @copyright (C) 2015, Mobile Arts AB
%%% @doc
%%% Leveldb Library functions.
%%% @end
%%% Created :  156 Feb 2015 by erdem <erdem@sitting>
%%%-------------------------------------------------------------------
-module(leveldb_lib).

%%API
-export([build_leveldb_options/1,
         build_leveldb_readoptions/1,
         build_leveldb_writeoptions/1]).

-include("leveldb.hrl").

%%%===================================================================
%%% API
%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Build a #leveldb_options{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_leveldb_options(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_leveldb_options(OptionsPL) ->
    build_leveldb_options(OptionsPL, #leveldb_options{}).

-spec build_leveldb_options(OptionsPL::[{atom(), term()}],
                            LeveldbOptions::#leveldb_options{}) -> ok | {error, Reason::term()}.
build_leveldb_options([], LeveldbOptions) ->
    LeveldbOptions;
build_leveldb_options([{comparator, ascending}|Rest], LeveldbOptions) ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{comparator = 1});
build_leveldb_options([{comparator, descending}|Rest], LeveldbOptions) ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{comparator = 0});
build_leveldb_options([{create_if_missing, Bool}|Rest], LeveldbOptions)
    when Bool == false; Bool == true ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{create_if_missing = Bool});
build_leveldb_options([{error_if_exists, Bool}|Rest], LeveldbOptions)
    when Bool == false; Bool == true ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{error_if_exists = Bool});
build_leveldb_options([{paranoid_checks, Bool}|Rest], LeveldbOptions)
    when Bool == false; Bool == true ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{paranoid_checks = Bool});
build_leveldb_options([{write_buffer_size, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{write_buffer_size = Int});
build_leveldb_options([{max_open_files, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{max_open_files = Int});
build_leveldb_options([{block_size, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{block_size = Int});
build_leveldb_options([{block_restart_interval, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_leveldb_options(Rest, LeveldbOptions#leveldb_options{block_restart_interval = Int});
build_leveldb_options([E|Rest], LeveldbOptions) ->
    error_logger:info_msg("Unsupported leveldb options parameter: ~p", [E]),
    build_leveldb_options(Rest, LeveldbOptions).


%%--------------------------------------------------------------------
%% @doc
%% Build a #leveldb_readptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_leveldb_readoptions(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_leveldb_readoptions(OptionsPL) ->
    build_leveldb_readoptions(OptionsPL, #leveldb_readoptions{}).

-spec build_leveldb_readoptions(OptionsPL::[{atom(), term()}],
                            LeveldbiReadOptions::#leveldb_readoptions{}) -> ok | {error, Reason::term()}.
build_leveldb_readoptions([], LeveldbReadOptions) ->
    LeveldbReadOptions;
build_leveldb_readoptions([{verify_checksums, Bool}|Rest],
                          LeveldbReadOptions) when Bool == true;
                                                   Bool == false ->
    build_leveldb_readoptions(Rest,
                              LeveldbReadOptions#leveldb_readoptions{verify_checksums = Bool});
build_leveldb_readoptions([{fill_cache, Bool}|Rest],
                          LeveldbReadOptions) when Bool == true;
                                                   Bool == false ->
    build_leveldb_readoptions(Rest,
                              LeveldbReadOptions#leveldb_readoptions{fill_cache = Bool});
build_leveldb_readoptions([_|Rest], LeveldbReadOptions) -> 
    build_leveldb_readoptions(Rest, LeveldbReadOptions).

%%--------------------------------------------------------------------
%% @doc
%% Build a #leveldb_writeptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_leveldb_writeoptions(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_leveldb_writeoptions(OptionsPL) ->
    build_leveldb_writeoptions(OptionsPL, #leveldb_writeoptions{}).

-spec build_leveldb_writeoptions(OptionsPL::[{atom(), term()}],
                                 LeveldbiReadOptions::#leveldb_writeoptions{}) -> ok | {error, Reason::term()}.
build_leveldb_writeoptions([], LeveldbWriteOptions) ->
    LeveldbWriteOptions;
build_leveldb_writeoptions([{sync, Bool}|Rest],
                           LeveldbWriteOptions) when Bool == true;
                                                     Bool == false ->
    build_leveldb_writeoptions(Rest,
                               LeveldbWriteOptions#leveldb_writeoptions{sync = Bool});
build_leveldb_writeoptions([_|Rest], LeveldbWriteOptions) -> 
    build_leveldb_writeoptions(Rest, LeveldbWriteOptions).
