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
Estimated total run time: 1.60 min

##### With input Small #####
Name                         ips        average  deviation         median         99th %
lru get                 575.36 K        1.74 μs  ±1071.13%        1.62 μs        2.47 μs
gen_server_lru get      513.71 K        1.95 μs  ±1064.76%        1.71 μs        2.90 μs

Comparison: 
lru get                 575.36 K
gen_server_lru get      513.71 K - 1.12x slower +0.21 μs

##### With input Medium #####
Name                         ips        average  deviation         median         99th %
lru get                 545.81 K        1.83 μs  ±1031.75%        1.63 μs        3.13 μs
gen_server_lru get      505.80 K        1.98 μs  ±1065.57%        1.73 μs        2.99 μs

Comparison: 
lru get                 545.81 K
gen_server_lru get      505.80 K - 1.08x slower +0.145 μs

##### With input Large #####
Name                         ips        average  deviation         median         99th %
gen_server_lru get      503.32 K        1.99 μs  ±1066.67%        1.72 μs        3.07 μs
lru get                 391.34 K        2.56 μs   ±563.15%        1.65 μs       11.32 μs

Comparison: 
gen_server_lru get      503.32 K
lru get                 391.34 K - 1.29x slower +0.57 μs

##### With input X-Large #####
Name                         ips        average  deviation         median         99th %
gen_server_lru get      489.02 K        2.04 μs  ±1047.05%        1.74 μs        3.24 μs
lru get                 100.91 K        9.91 μs   ±242.78%        1.68 μs       96.79 μs

Comparison: 
gen_server_lru get      489.02 K
lru get                 100.91 K - 4.85x slower +7.86 μs
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
Estimated total run time: 1.60 min

##### With input Small #####
Name                         ips        average  deviation         median         99th %
lru put                 482.11 K        2.07 μs   ±891.31%        1.91 μs        3.30 μs
gen_server_lru put      403.32 K        2.48 μs   ±792.89%        2.41 μs        3.37 μs

Comparison: 
lru put                 482.11 K
gen_server_lru put      403.32 K - 1.20x slower +0.41 μs

##### With input Medium #####
Name                         ips        average  deviation         median         99th %
gen_server_lru put      397.81 K        2.51 μs   ±791.75%        2.44 μs        3.43 μs
lru put                 243.33 K        4.11 μs   ±347.72%        3.95 μs        8.15 μs

Comparison: 
gen_server_lru put      397.81 K
lru put                 243.33 K - 1.63x slower +1.60 μs

##### With input Large #####
Name                         ips        average  deviation         median         99th %
gen_server_lru put      390.46 K        2.56 μs   ±762.96%        2.51 μs        3.48 μs
lru put                  46.90 K       21.32 μs    ±42.40%       22.56 μs       38.47 μs

Comparison: 
gen_server_lru put      390.46 K
lru put                  46.90 K - 8.33x slower +18.76 μs

##### With input X-Large #####
Name                         ips        average  deviation         median         99th %
gen_server_lru put      376.58 K        2.66 μs   ±757.67%        2.59 μs        3.67 μs
lru put                   4.80 K      208.37 μs    ±30.77%      223.17 μs      315.53 μs

Comparison: 
gen_server_lru put      376.58 K
lru put                   4.80 K - 78.47x slower +205.72 μs
```

### Limitations

Benchee does not track memory usage outside of the runner process, which
disallows us from measuring meaningful memory metrics.

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
