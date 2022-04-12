%% @doc A Least-Recently Used (LRU) cache server
%%
%% This one is implemented entirely with ets.
-module(ets_lru).

-record(cache, {cache :: ets:t(),
                ranks :: ets:t(),
                capacity :: pos_integer()}).

-export([get/3, put/3, init/1, clear/1]).

-spec get(Cache :: #cache{}, Key :: term(), Default :: term()) -> Value :: term().
%% @doc Gets the value in the cache, giving Default if not found

get(Cache, Key, Default) ->
    case ets:lookup(Cache#cache.cache, Key) of
        [{Key, Rank, Value}] ->
            update_rank(Cache, Rank, Key),
            Value;
        [] ->
            Default
    end.

-spec put(Cache :: #cache{}, Key :: term(), Value :: term()) -> ok.
%% @doc Puts a value into the cache with a given key

put(Cache, Key, Value) ->
    case ets:lookup(Cache#cache.cache, Key) of
        [{Key, _Rank, Value}] ->
            ok;
        [{Key, Rank, _OtherValue}] ->
            update_rank(Cache, Rank, Key),
            ets:update_element(Cache#cache.cache, Key, {3, Value}),
            ok;
        [] ->
            insert_key(Cache, Key, Value),
            ok
    end.

-spec init(Capacity :: pos_integer()) -> #cache{}.

init(Capacity) ->
    Cache = ets:new(cache, [public, set]),
    Ranks = ets:new(ranks, [public, ordered_set]),
    #cache{cache = Cache, ranks = Ranks, capacity = Capacity}.

-spec clear(Cache :: #cache{}) -> ok.

clear(Cache) ->
    ets:delete(Cache#cache.cache),
    ets:delete(Cache#cache.ranks),
    ok.

update_rank(Cache, Rank, Key) ->
    NextRank = next_rank(),
    ets:delete(Cache#cache.ranks, Rank),
    ets:insert(Cache#cache.ranks, {NextRank, Key}),
    ets:update_element(Cache#cache.cache, Key, {2, NextRank}),
    ok.

insert_key(Cache, Key, Value) ->
    case ets:info(Cache#cache.cache, size) >= Cache#cache.capacity of
        true ->
            %% The cache is full so we must discard the least recently used
            %% item.
            OldestRank = ets:first(Cache#cache.ranks),
            [{OldestRank, OldestKey}] = ets:take(Cache#cache.ranks, OldestRank),
            ets:delete(Cache#cache.cache, OldestKey);
        _ ->
            ok
    end,
    %% Insert the new element into the cache.
    NextRank = next_rank(),
    ets:insert(Cache#cache.cache, {Key, NextRank, Value}),
    ets:insert(Cache#cache.ranks, {NextRank, Key}),
    ok.

next_rank() ->
    erlang:unique_integer([monotonic]).
