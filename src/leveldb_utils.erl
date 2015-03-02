-module(leveldb_utils).

-export([merge_sorted_kvls/1]).

-on_load(init/0).

-type value() :: binary() | string().
-type key() :: binary() | string().

init() ->
Dir = "../priv",
    PrivDir =
    case code:priv_dir(leveldb) of
        {error, _} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename), Dir]);
                _ ->
                    Dir
            end;
        Path -> Path
    end,
    Lib = filename:join(PrivDir, "leveldb_utils"),
    erlang:load_nif(Lib, 0).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Merge the already sorted Key/Value tuple lists those are
%% returned from leveldb:read_range/4 calls.
%% @end
%%--------------------------------------------------------------------
-spec merge_sorted_kvls(KVLs :: [[{key(), value()}]]) -> {ok, [{key(), value()}]} | {error, Reason :: any()}.
merge_sorted_kvls(KVLs)->
    erlang:nif_error(nif_library_not_loaded).

