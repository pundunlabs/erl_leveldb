-module(leveldb_utils).

-export([merge_sorted_kvls/1]).

-export([test/1,
	 test_lists/1,
	 test_lists2/1,
	 test_get_lists/2,
	 test_get_lists/3,
	 test_merge_lists/2]).

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
merge_sorted_kvls(_KVLs)->
    erlang:nif_error(nif_library_not_loaded).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Test functions.
%% Tests for merge sorted key value lists.
%% @end
%%--------------------------------------------------------------------
test(N) ->
    [Odd, Even] = test_get_lists(1,N),
    {[Odd, Even], merge_sorted_kvls([Odd, Even])}.

test_lists(Lists) ->
    timer:tc(?MODULE, merge_sorted_kvls, [Lists]).

test_lists2(Lists) ->
    timer:tc(?MODULE, test_merge_lists, [Lists, []]).

test_get_lists(Start, Stop) ->
    Odd  = test_get_modular_seq(Start, Stop, 2, 1),
    Even = test_get_modular_seq(Start, Stop, 2, 0),
    [Odd, Even].

test_get_modular_seq(Start, Stop, Mod, Rem) ->
    [ begin Str = list_to_binary(integer_to_list(X)),
	   {Str,<<"Data">>} end || X <- lists:seq(Stop, Start, -1), X rem Mod == Rem].
    

test_get_lists(Start, Stop, Mod) ->
    [test_get_modular_seq(Start, Stop, Mod, R)|| R <- lists:seq(0, Mod-1)].

test_merge_lists([H | Rest], []) ->
    test_merge_lists(Rest, H);
test_merge_lists([H | Rest], Aux) ->
    test_merge_lists(Rest, lists:keymerge(1, H, Aux));
test_merge_lists([], Aux) ->
    Aux.
    
    
    
