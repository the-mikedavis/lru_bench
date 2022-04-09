%% @doc A Least-Recently Used (LRU) cache server
%%
%% This one is implemented as a gen_server which uses `ets' for all data.
-module(ets_lru2).

-behaviour(gen_server).

-export([get/2, put/2, start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2]).

-record(state,
        {id = 0 :: non_neg_integer(),
         capacity :: pos_integer(),
         cardinality = 0 :: non_neg_integer(),
         keys_to_values :: ets:t(),
         keys_to_ids :: ets:t(),
         ids_to_keys :: ets:t()}).

-spec get(Key :: term(), Default :: term()) -> Value :: term().
%% @doc Gets the value in the cache, giving Default if not found

get(Key, Default) ->
    gen_server:call(?MODULE, {get, Key, Default}).

-spec put(Key :: term(), Value :: term()) -> ok.
%% @doc Puts a value into the cache with a given key

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

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
%% application:get_env(ets_lru, capacity, 10_000).
%% '''
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @private
init(Opts) ->
    Capacity =
        case proplists:get_value(capacity, Opts) of
            undefined ->
                application:get_env(ets_lru, capacity, 10_000);
            Cap ->
                Cap
        end,
    case Capacity of
        none ->
            ignore;
        _ ->
            IdsToKeys = ets:new(ets_lru_ids_to_keys, [ordered_set, private]),
            KeysToIds = ets:new(ets_lru_keys_to_ids, [set, private]),
            KeysToValues = ets:new(ets_lru_keys_to_values, [set, private]),
            {ok, #state{capacity = Capacity,
                        ids_to_keys = IdsToKeys,
                        keys_to_ids = KeysToIds,
                        keys_to_values = KeysToValues}}
    end.

%% @private
handle_call({get, Key, Default}, _From, State) ->
    case ets:lookup(State#state.keys_to_ids, Key) of
        [] ->
            {reply, Default, State};
        [{Key, CurrentId}] ->
            % Increment the access ID for the entry
            NextId = State#state.id + 1,
            _ = ets:update_element(State#state.keys_to_ids, Key, {2, NextId}),
            _ = ets:delete(State#state.ids_to_keys, CurrentId),
            _ = ets:insert(State#state.ids_to_keys, {NextId, Key}),
            [{Key, Value}] = ets:lookup(State#state.keys_to_values, Key),
            {reply, Value, State#state{id = NextId}}
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
    %% discard the least recently used element. The `?IDS_TO_KEYS' table
    %% is an ordered set, so the insert is logarithmic in time.
    State1 =
        case State#state.cardinality of
            Capacity ->
                OldestId = ets:first(State#state.ids_to_keys),
                [{OldestId, OldestKey}] = ets:lookup(State#state.ids_to_keys, OldestId),
                _ = ets:delete(State#state.ids_to_keys, OldestId),
                _ = ets:delete(State#state.keys_to_ids, OldestKey),
                _ = ets:delete(State#state.keys_to_values, OldestKey),
                State#state{cardinality = State#state.cardinality - 1};
            _ ->
                State
        end,
    %% Insert the new element into the cache.
    NextId = State1#state.id + 1,
    _ = ets:insert(State#state.ids_to_keys, {NextId, Key}),
    _ = ets:insert(State#state.keys_to_ids, {Key, NextId}),
    _ = ets:insert(State#state.keys_to_values, {Key, Value}),
    State1#state{id = NextId,
                 cardinality = State1#state.cardinality + 1}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Msg, State) ->
    {noreply, State}.
