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
-export([build_leveldb_options/1]).

-include("leveldb.hrl").

%%%===================================================================
%%% API
%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Build a #leveldb_options{} record with the provided proplist OptionsPL
%%
%%--------------------------------------------------------------------
-spec build_leveldb_options(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_leveldb_options(OptionsPL) ->
    build_leveldb_options(OptionsPL, #leveldb_options{}).

-spec build_leveldb_options(OptionsPL::[{atom(), term()}],
                            LeveldbOptions::#leveldb_options{}) -> ok | {error, Reason::term()}.
build_leveldb_options([], LeveldbOptions) ->
    LeveldbOptions;
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

 
