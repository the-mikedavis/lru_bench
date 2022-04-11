# LruBench

A benchmark comparison of some LRU cache approaches in Erlang

The caches are implemented in `src/` and a benchmarking script in `bench.exs`
runs the Benchee benchmarks.

### Results

Note: the "inputs" are used to control the capacity of the cache.

* Small - 10
* Medium - 100
* Large - 1,000
* X-Large - 10,000

##### Get

```
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-9700KF CPU @ 3.60GHz
Number of Available Cores: 8
Available memory: 31.30 GB
Elixir 1.13.3
Erlang 24.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 10 s
memory time: 0 ns
reduction time: 0 ns
parallel: 1
inputs: Small, Medium, Large, X-Large
Estimated total run time: 2.40 min

##### With input Small #####
Name                         ips        average  deviation         median         99th %
ets_lru get            2396.55 K        0.42 μs  ±8960.45%        0.30 μs        0.63 μs
lru get                 604.65 K        1.65 μs  ±1024.00%        1.53 μs        2.44 μs
gen_server_lru get      522.15 K        1.92 μs  ±1004.84%        1.67 μs        2.85 μs

Comparison: 
ets_lru get            2396.55 K
lru get                 604.65 K - 3.96x slower +1.24 μs
gen_server_lru get      522.15 K - 4.59x slower +1.50 μs

##### With input Medium #####
Name                         ips        average  deviation         median         99th %
ets_lru get            2383.16 K        0.42 μs  ±8409.66%        0.31 μs        0.56 μs
lru get                 572.01 K        1.75 μs   ±974.96%        1.55 μs        3.05 μs
gen_server_lru get      517.61 K        1.93 μs   ±994.94%        1.66 μs        2.98 μs

Comparison: 
ets_lru get            2383.16 K
lru get                 572.01 K - 4.17x slower +1.33 μs
gen_server_lru get      517.61 K - 4.60x slower +1.51 μs

##### With input Large #####
Name                         ips        average  deviation         median         99th %
ets_lru get            2303.01 K        0.43 μs  ±7878.63%        0.32 μs        0.61 μs
gen_server_lru get      523.77 K        1.91 μs   ±930.89%        1.65 μs        3.01 μs
lru get                 398.58 K        2.51 μs   ±611.82%        1.58 μs       11.37 μs

Comparison: 
ets_lru get            2303.01 K
gen_server_lru get      523.77 K - 4.40x slower +1.48 μs
lru get                 398.58 K - 5.78x slower +2.07 μs

##### With input X-Large #####
Name                         ips        average  deviation         median         99th %
ets_lru get            2266.93 K        0.44 μs  ±7421.29%        0.33 μs        0.64 μs
gen_server_lru get      507.95 K        1.97 μs   ±936.14%        1.67 μs        3.16 μs
lru get                  99.73 K       10.03 μs   ±241.55%        1.63 μs       98.06 μs

Comparison: 
ets_lru get            2266.93 K
gen_server_lru get      507.95 K - 4.46x slower +1.53 μs
lru get                  99.73 K - 22.73x slower +9.59 μs
```

##### Put

```
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-9700KF CPU @ 3.60GHz
Number of Available Cores: 8
Available memory: 31.30 GB
Elixir 1.13.3
Erlang 24.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 10 s
memory time: 0 ns
reduction time: 0 ns
parallel: 1
inputs: Small, Medium, Large, X-Large
Estimated total run time: 2.40 min

Benchmarking ets_lru put with input Small ...
Benchmarking ets_lru put with input Medium ...
Benchmarking ets_lru put with input Large ...
Benchmarking ets_lru put with input X-Large ...
Benchmarking gen_server_lru put with input Small ...
Benchmarking gen_server_lru put with input Medium ...
Benchmarking gen_server_lru put with input Large ...
Benchmarking gen_server_lru put with input X-Large ...
Benchmarking lru put with input Small ...
Benchmarking lru put with input Medium ...
Benchmarking lru put with input Large ...
Benchmarking lru put with input X-Large ...

##### With input Small #####
Name                         ips        average  deviation         median         99th %
lru put                 508.09 K        1.97 μs   ±917.45%        1.82 μs        3.07 μs
gen_server_lru put      409.24 K        2.44 μs   ±781.19%        2.37 μs        3.87 μs
ets_lru put             376.55 K        2.66 μs   ±579.75%        3.03 μs        3.42 μs

Comparison: 
lru put                 508.09 K
gen_server_lru put      409.24 K - 1.24x slower +0.48 μs
ets_lru put             376.55 K - 1.35x slower +0.69 μs

##### With input Medium #####
Name                         ips        average  deviation         median         99th %
gen_server_lru put      404.90 K        2.47 μs   ±771.95%        2.41 μs        3.78 μs
lru put                 251.46 K        3.98 μs   ±341.45%        3.85 μs        7.87 μs
ets_lru put              68.34 K       14.63 μs    ±67.01%       17.69 μs       20.98 μs

Comparison: 
gen_server_lru put      404.90 K
lru put                 251.46 K - 1.61x slower +1.51 μs
ets_lru put              68.34 K - 5.92x slower +12.16 μs

##### With input Large #####
Name                         ips        average  deviation         median         99th %
gen_server_lru put      396.43 K        2.52 μs   ±769.90%        2.47 μs        3.46 μs
lru put                  47.50 K       21.05 μs    ±43.14%       22.31 μs       36.19 μs
ets_lru put               7.39 K      135.24 μs    ±50.25%      166.69 μs      205.61 μs

Comparison: 
gen_server_lru put      396.43 K
lru put                  47.50 K - 8.35x slower +18.53 μs
ets_lru put               7.39 K - 53.61x slower +132.72 μs

##### With input X-Large #####
Name                         ips        average  deviation         median         99th %
gen_server_lru put      381.42 K        2.62 μs   ±758.32%        2.55 μs        3.64 μs
lru put                   4.93 K      203.03 μs    ±30.85%      218.51 μs      298.93 μs
ets_lru put               0.69 K     1451.46 μs    ±48.70%     1775.32 μs     1970.10 μs

Comparison: 
gen_server_lru put      381.42 K
lru put                   4.93 K - 77.44x slower +200.41 μs
ets_lru put               0.69 K - 553.61x slower +1448.84 μs
```

### Limitations

Benchee does not track memory usage outside of the runner process, which
disallows us from measuring meaningful memory metrics in caches implemented
as gen_servers.

### Background

The `lru` case uses the popular `lru` package on hex
([source](https://gitlab.com/barrel-db/erlang-lru/-/tree/master)). This
implementation does not use `ets` at all: instead elements are stored in
a map and the expiration values are stored in a list.

### Discussion

With a very small cache, `lru` outperforms the implementation here.
`lru`'s performance degrades as `Capacity` is increased though. This can
be explained by `lru`'s use of a list for the tracking of expriation data.
For example, take a common case where the LRU is full and a new element is
inserted: the least-recently used element in the cache must be evicted.
`lru`'s implementation looks somewhat like so:

```erl
remove_oldest(Cache) ->
  Last = lists:last(Cache#cache.evict_list),
  Cache#cache{evict_list=lists:droplast(Cache#cache.evict_list),
              items=maps:remove(Last, Cache#cache.items)}.
```

The `lists:last/1` and `lists:droplast/1` are linear on the length of the
list, and the length of the list is `Capacity` when the cache is full.

In the implementation here, though, we use `ets:first/1`

```erl
OldestId = ets:first(State#state.ids_to_keys),
[{OldestId, OldestKey}] = ets:lookup(State#state.ids_to_keys, OldestId),
_ = ets:delete(State#state.ids_to_keys, OldestId),
_ = ets:delete(State#state.keys_to_ids, OldestKey),
_ = ets:delete(State#state.keys_to_values, OldestKey),
```

The `State#state.ids_to_keys` ets table is an `ordered_set`, so lookup,
insertion, and deletion, and determining `ets:first/1` in the set are
logarithmic on the size of the set, and the set has `Capacity` elements
when the cache is full.

A similar situation occurs when getting an element from the cache. In
all implementations, lookup of the value for a given key is very fast:
either an `ets:lookup/2` in a `set` or a `maps:get/2`. For the sake
of argument, let's call those lookups roughly constant-time. The expensive
part of getting an element from an LRU cache is updating the metadata
that tracks which keys have been least-recently used. In `lru`, this
is straightforward with a list:

```erl
move_front(List, Key) ->
  [Key | lists:delete(Key, List)].
```

In the ets-based implementation though, we update the "ID" (a
incrementing integer which identifies the order in which keys were
inserted and accessed):

```erl
_ = ets:update_element(State#state.keys_to_ids, Key, {2, NextId}),
_ = ets:delete(State#state.ids_to_keys, CurrentId),
_ = ets:insert(State#state.ids_to_keys, {NextId, Key}),
```

For the `State#state.keys_to_ids` table which is a `set`, we can say that the
update is roughly constant-time. The `State#state.ids_to_keys` `ordered_set`
table has a logarithmic update time which dominates the other update. This
logarithmic update time scales better than the linear `move_front/2`
implementation.

So we see that the ets-based implementation here is logarithmic for both
`put/2` and `get/2`, while the `lru` implementation is linear on the
`Capacity` of the cache.

### Licence

This work is licensed under the BSD 0-Clause License. See the `LICENSE` file
for more information.
