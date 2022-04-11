%% @doc A Least-Recently Used (LRU) cache server
%%
%% This one is implemented entirely with ets.
-module(ets_lru).

-export([get/2, put/2, init/1, clear/0]).

-define(CACHE, '$__ETS_LRU_CACHE__').
-define(RANKS, '$__ETS_LRU_RANKS__').

-spec get(Key :: term(), Default :: term()) -> Value :: term().
%% @doc Gets the value in the cache, giving Default if not found

get(Key, Default) ->
    case ets:lookup(?CACHE, Key) of
        [{Key, Rank, Value}] ->
            update_rank(Rank, Key),
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
        [{Key, Rank, _OtherValue}] ->
            update_rank(Rank, Key),
            ok;
        [] ->
            insert_key(Key, Value),
            ok
    end.

init(Capacity) ->
    _ = application:set_env(ets_lru, capacity, Capacity),
    _ = ets:new(?CACHE, [public, named_table, set]),
    _ = ets:new(?RANKS, [public, ordered_set, named_table]),
    ok.

clear() ->
    ets:delete(?CACHE),
    ets:delete(?RANKS),
    ok.

update_rank(Rank, Key) ->
    NextRank = next_rank(),
    _ = ets:delete(?RANKS, Rank),
    _ = ets:insert(?RANKS, {NextRank, Key}),
    _ = ets:update_element(?CACHE, Key, {2, NextRank}),
    ok.

insert_key(Key, Value) ->
    case ets:info(?CACHE, size) >= capacity() of
        true ->
            %% The cache is full so we must discard the least recently used
            %% item.
            OldestRank = ets:first(?RANKS),
            [{OldestRank, OldestKey}] = ets:take(?RANKS, OldestRank),
            ets:delete(?CACHE, OldestKey);
        _ ->
            ok
    end,
    %% Insert the new element into the cache.
    NextRank = next_rank(),
    ets:insert(?CACHE, {Key, NextRank, Value}),
    ets:insert(?RANKS, {NextRank, Key}),
    ok.

capacity() ->
    application:get_env(ets_lru, capacity, 10_000).

next_rank() ->
    erlang:unique_integer([monotonic]).
