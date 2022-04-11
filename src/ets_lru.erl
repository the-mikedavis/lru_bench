%% @doc A Least-Recently Used (LRU) cache server
%%
%% This one is implemented entirely with ets.
-module(ets_lru).

-export([get/2, put/2, init/1, clear/0]).

-define(CACHE, '$__ETS_LRU_CACHE__').
%% In Erlang term ordering, a 1-arity tuple is `>' than all integers.
-define(INT_INFINITY, {1}).

-spec get(Key :: term(), Default :: term()) -> Value :: term().
%% @doc Gets the value in the cache, giving Default if not found

get(Key, Default) ->
    case ets:lookup(?CACHE, Key) of
        [{Key, _Rank, Value}] ->
            _ = ets:update_element(?CACHE, Key, {2, next_rank()}),
            Value;
        [] ->
            Default
    end.

-spec put(Key :: term(), Value :: term()) -> ok.
%% @doc Puts a value into the cache with a given key

put(Key, Value) ->
    case ets:lookup(?CACHE, Key) of
        [{Key, _Rank, Value}] ->
            ok;
        [{Key, _Rank, _OtherValue}] ->
            _ = ets:update_element(?CACHE, Key, [{2, next_rank()}, {3, Value}]),
            ok;
        [] ->
            insert_key(Key, Value),
            ok
    end.

init(Capacity) ->
    _ = application:set_env(ets_lru, capacity, Capacity),
    _ = ets:new(?CACHE, [public, set, named_table]),
    ok.

clear() ->
    ets:delete(?CACHE),
    ok.

insert_key(Key, Value) ->
    %% Ensure the cache has capacity. If the cache has reached capacity,
    %% discard the least recently used element. The `?IDS_TO_KEYS' table
    %% is an ordered set, so the insert is logarithmic in time.
    case ets:info(?CACHE, size) >= capacity() of
        true ->
            %% The cache is full so we must discard the least recently used
            %% item.
            {OldestKey, _Rank, _Value} =
                ets:foldl(fun({_Key, Rank, _Value} = Element, Acc) when Rank < Acc -> Element;
                             (_Element, Acc)                                       -> Acc
                          end, ?INT_INFINITY, ?CACHE),
            ets:delete(?CACHE, OldestKey);
        _ ->
            ok
    end,
    %% Insert the new element into the cache.
    ets:insert(?CACHE, {Key, next_rank(), Value}).

capacity() ->
    application:get_env(ets_lru, capacity, 10_000).

next_rank() ->
    erlang:unique_integer([monotonic]).
