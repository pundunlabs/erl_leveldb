-module(leveldb_test).

-revision('$Revision: $ ').
-modified('$Date: $ ').

-include("leveldb.hrl").

%% API
-export([basic/0,
	 open_close/0,
	 put_get_delete/1,
	 put_n/1,
	 get_x/1,
	 write_n/1]).

-export([open_db/1,
	 close_db/1,
	 get/2,
	 put/3,
	 delete/2]).

-export([options/0,
	 readoptions/0,
	 writeoptions/0]).

-export([resource_test_n/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
basic()->
    {ok, Options} = options(),
    {ok, DB} = leveldb:open_db(Options, "/tmp/basicdb"),
    ok = put(DB, erlang:term_to_binary("1"), erlang:term_to_binary("some example data")),
    {ok, Value} = get(DB, erlang:term_to_binary("1")),
    io:format("Value: ~p~n",[erlang:binary_to_term(Value)]),
    ok = delete(DB, erlang:term_to_binary("1")),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
open_close()->
    {ok, Options} = options(),
    {ok, DB} = leveldb:open_db(Options, "/tmp/basicdb"),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
put_get_delete(N)->
    {ok, Options} = options(),
    {ok, DB} = leveldb:open_db(Options, "/tmp/basicdb"),
    PGD =
	fun(X)->
		ok = put(DB, erlang:term_to_binary(X),
			 erlang:term_to_binary("some example data")),
		{ok, _Value} = get(DB, erlang:term_to_binary(X)),
		ok = delete(DB, erlang:term_to_binary(X))
	end,
    lists:map(PGD, lists:seq(1,N)),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
put_n(N)->
    {ok, Options} = options(),
    {ok, DB} = leveldb:open_db(Options, "/tmp/basicdb"),
    {ok, WriteOptions} = writeoptions(),
    Put =
	fun(X)->
		ok = leveldb:put(DB, WriteOptions,
				 erlang:term_to_binary(X),
				 erlang:term_to_binary("some example data " ++ integer_to_list(X))
				)
	end,
    lists:map(Put, lists:seq(1,N)),
    erlang:garbage_collect(self()),
    erlang:garbage_collect(),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_x(X)->
    {ok, Options} = options(),
    {ok, DB} = leveldb:open_db(Options, "/tmp/basicdb"),
    {ok, ReadOptions} = readoptions(),
    Result= leveldb:get(DB, ReadOptions,
			erlang:term_to_binary(X)),
    ok = close_db(DB),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
write_n(N)->
    {ok, Options} = options(),
    {ok, DB} = leveldb:open_db(Options, "/tmp/basicdb"),
    {ok, WriteOptions} = writeoptions(),
    Batch =
	fun(X, {DK,PKV})->
		BK = erlang:term_to_binary(integer_to_list(X)),
		String = lists:concat(["some example data number: ", X]),
		BV = erlang:term_to_binary(String),
		{[BK|DK],[{BK,BV}|PKV]}
	end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,N)),
    ok = leveldb:write(DB, WriteOptions,
		       DeleteKeys, PutKVS),
    [begin {ok, _Value} = get(DB, Keys) end || Keys <- DeleteKeys],
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
open_db(Path)->
    {ok, Options} = options(),
    case leveldb:open_db(Options, Path) of
	{error, Reason} ->
	    {error, Reason};
	{ok, DB} ->
	    {ok, DB}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
close_db(DB)->
    ok = leveldb:close_db(DB).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get(DB, Key)->
    {ok, ReadOptions} = readoptions(),
    {ok, _Value} = leveldb:get(DB, ReadOptions, Key).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
put(DB, Key, Value)->
    {ok, WriteOptions} = writeoptions(),
    ok = leveldb:put(DB, WriteOptions, Key, Value).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete(DB, Key)->
    {ok, WriteOptions} = writeoptions(),
    ok = leveldb:delete(DB, WriteOptions, Key).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
options()->
    leveldb:options(#leveldb_options{create_if_missing=true}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
readoptions()->
    leveldb:readoptions(#leveldb_readoptions{}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
writeoptions()->
    leveldb:writeoptions(#leveldb_writeoptions{}).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
resource_test_n(N)->
    Put =
	fun(_)->
		leveldb:resource_test()
	end,
    lists:map(Put, lists:seq(1,N)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
