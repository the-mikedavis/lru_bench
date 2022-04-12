%% @doc A Least-Recently Used (LRU) cache server
%%
%% This one is implemented as a gen_server which uses `ets' for all data.
-module(gen_server_lru).

-behaviour(gen_server).

-export([get/3, put/3, start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2]).

-record(state,
        {rank = 0 :: non_neg_integer(),
         capacity :: pos_integer(),
         cardinality = 0 :: non_neg_integer(),
         keys_to_values :: ets:t(),
         keys_to_ranks :: ets:t(),
         ranks_to_keys :: ets:t()}).

-spec get(Cache :: pid(), Key :: term(), Default :: term()) -> Value :: term().
%% @doc Gets the value in the cache, giving Default if not found

get(Cache, Key, Default) ->
    gen_server:call(Cache, {get, Key, Default}).

-spec put(Cache :: pid(), Key :: term(), Value :: term()) -> ok.
%% @doc Puts a value into the cache with a given key

put(Cache, Key, Value) ->
    gen_server:call(Cache, {put, Key, Value}).

%% @doc Starts the LRU gen_server
%%
%% Options:
%%
%% <ul>
%% <li>
%% `capacity': the maximum number of elements that may be stored in the cache.
%% </li>
%% </ul>
%%
%% If the `capacity' option is not set, it will be fetched using
%%
%% ```
%% application:get_env(gen_server_lru, capacity, 10_000).
%% '''
start_link(Opts) ->
    gen:start(gen_server, link, ?MODULE, Opts, []).

%% @private
init(Opts) ->
    Capacity =
        case proplists:get_value(capacity, Opts) of
            undefined ->
                application:get_env(gen_server_lru, capacity, 10_000);
            Cap ->
                Cap
        end,
    case Capacity of
        none ->
            ignore;
        _ ->
            RanksToKeys = ets:new(gen_server_lru_ranks_to_keys, [ordered_set, private]),
            KeysToRanks = ets:new(gen_server_lru_keys_to_ranks, [set, private]),
            KeysToValues = ets:new(gen_server_lru_keys_to_values, [set, private]),
            {ok, #state{capacity = Capacity,
                        ranks_to_keys = RanksToKeys,
                        keys_to_ranks = KeysToRanks,
                        keys_to_values = KeysToValues}}
    end.

%% @private
handle_call({get, Key, Default}, _From, State) ->
    case ets:lookup(State#state.keys_to_ranks, Key) of
        [] ->
            {reply, Default, State};
        [{Key, CurrentRank}] ->
            % Refresh the rank for the entry
            NextRank = State#state.rank + 1,
            [{Key, Value}] = ets:lookup(State#state.keys_to_values, Key),
            _ = ets:update_element(State#state.keys_to_ranks, Key, {2, NextRank}),
            _ = ets:delete(State#state.ranks_to_keys, CurrentRank),
            _ = ets:insert(State#state.ranks_to_keys, {NextRank, Key}),
            {reply, Value, State#state{rank = NextRank}}
    end;
handle_call({put, Key, Value}, _From, State) ->
    case ets:lookup(State#state.keys_to_values, Key) of
        [] ->
            State1 = insert_key(State, Key, Value),
            {reply, ok, State1};
        [{Key, Value}] ->
            {reply, ok, State};
        [_OtherValue] ->
            ets:insert(State#state.keys_to_values, {Key, Value}),
            {reply, ok, State}
    end.

insert_key(#state{capacity = Capacity} = State, Key, Value) ->
    %% Ensure the cache has capacity. If the cache has reached capacity,
    %% discard the least recently used element. The `ranks_to_keys' table
    %% is an ordered set, so the insert is logarithmic in time.
    State1 =
        case State#state.cardinality of
            Capacity ->
                OldestRank = ets:first(State#state.ranks_to_keys),
                [{OldestRank, OldestKey}] = ets:lookup(State#state.ranks_to_keys, OldestRank),
                _ = ets:delete(State#state.ranks_to_keys, OldestRank),
                _ = ets:delete(State#state.keys_to_ranks, OldestKey),
                _ = ets:delete(State#state.keys_to_values, OldestKey),
                State#state{cardinality = State#state.cardinality - 1};
            _ ->
                State
        end,
    %% Insert the new element into the cache.
    NextRank = State1#state.rank + 1,
    _ = ets:insert(State#state.ranks_to_keys, {NextRank, Key}),
    _ = ets:insert(State#state.keys_to_ranks, {Key, NextRank}),
    _ = ets:insert(State#state.keys_to_values, {Key, Value}),
    State1#state{rank = NextRank,
                 cardinality = State1#state.cardinality + 1}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Msg, State) ->
    {noreply, State}.
