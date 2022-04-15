%% @doc A Least-Recently Used (LRU) cache server
%%
%% This one is implemented as a gen_server which uses a `gb_tree' for holding
%% rank information and maps otherwise.
-module(gen_server_gb_tree_lru).

-behaviour(gen_server).

-export([get/3, put/3, start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2]).

-record(state,
        {rank = 0 :: non_neg_integer(),
         capacity :: pos_integer(),
         cardinality = 0 :: non_neg_integer(),
         keys_to_values = #{} :: map:t(),
         keys_to_ranks = #{} :: map:t(),
         ranks_to_keys = gb_trees:empty() :: gb_trees:t()}).

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
%% application:get_env(gen_server_ets_lru, capacity, 10_000).
%% '''
start_link(Opts) ->
    gen:start(gen_server, link, ?MODULE, Opts, []).

%% @private
init(Opts) ->
    Capacity =
        case proplists:get_value(capacity, Opts) of
            undefined ->
                application:get_env(gen_server_ets_lru, capacity, 10_000);
            Cap ->
                Cap
        end,
    case Capacity of
        none ->
            ignore;
        _ ->
            {ok, #state{capacity = Capacity}}
    end.

%% @private
handle_call({get, Key, Default}, _From, State) ->
    case maps:find(Key, State#state.keys_to_ranks) of
        error ->
            {reply, Default, State};
        {ok, CurrentRank} ->
            % Refresh the rank for the entry
            NextRank = State#state.rank + 1,
            KeysToRanks = maps:put(Key, NextRank, State#state.keys_to_ranks),
            RanksToKeys0 = gb_trees:delete(CurrentRank, State#state.ranks_to_keys),
            RanksToKeys1 = gb_trees:insert(NextRank, Key, RanksToKeys0),
            Value = maps:get(Key, State#state.keys_to_values),
            {reply, Value, State#state{rank = NextRank,
                                       keys_to_ranks = KeysToRanks,
                                       ranks_to_keys = RanksToKeys1}}
    end;
handle_call({put, Key, Value}, _From, State) ->
    case maps:find(Key, State#state.keys_to_values) of
        error ->
            State1 = insert_key(State, Key, Value),
            {reply, ok, State1};
        {ok, Value} ->
            {reply, ok, State};
        {ok, _OtherValue} ->
            maps:put(Key, Value, State#state.keys_to_values),
            {reply, ok, State}
    end.

insert_key(#state{capacity = Capacity} = State, Key, Value) ->
    %% Ensure the cache has capacity. If the cache has reached capacity,
    %% discard the least recently used element. The `ranks_to_keys' table
    %% is an ordered set, so the insert is logarithmic in time.
    State1 =
        case State#state.cardinality of
            Capacity ->
                {_OldestRank, OldestKey, RanksToKeys} =
                  gb_trees:take_smallest(State#state.ranks_to_keys),
                KeysToRanks = maps:remove(OldestKey, State#state.keys_to_ranks),
                KeysToValues = maps:remove(OldestKey, State#state.keys_to_values),
                State#state{cardinality = State#state.cardinality - 1,
                            ranks_to_keys = RanksToKeys,
                            keys_to_ranks = KeysToRanks,
                            keys_to_values = KeysToValues};
            _ ->
                State
        end,
    %% Insert the new element into the cache.
    NextRank = State1#state.rank + 1,
    RanksToKeys1 = gb_trees:insert(NextRank, Key, State1#state.ranks_to_keys),
    KeysToRanks1 = maps:put(Key, NextRank, State1#state.keys_to_ranks),
    KeysToValues1 = maps:put(Key, Value, State1#state.keys_to_values),
    State1#state{rank = NextRank,
                 cardinality = State1#state.cardinality + 1,
                 ranks_to_keys = RanksToKeys1,
                 keys_to_ranks = KeysToRanks1,
                 keys_to_values = KeysToValues1}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Msg, State) ->
    {noreply, State}.
