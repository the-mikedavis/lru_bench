# LruBench

A benchmark comparison of some LRU cache approaches in Erlang

The caches are implemented in `src/`.

### Results

Note: the "inputs" are used to control the capacity of the cache.

* Small - 10
* Medium - 100
* Large - 1,000
* X-Large - 10,000
* XX-Large - 100,000

`persistent_term` is given as a baseline.

##### Get

```
Generated lru_bench app
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-9700KF CPU @ 3.60GHz
Number of Available Cores: 8
Available memory: 31.30 GB
Elixir 1.13.4
Erlang 24.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 5 s
memory time: 0 ns
reduction time: 0 ns
parallel: 4
inputs: Small, Medium, Large, X-Large, XX-Large
Estimated total run time: 2.92 min

##### With input Small #####
Name                                 ips        average  deviation         median         99th %
persistent_term get            2378.55 K        0.42 μs  ±5894.72%        0.34 μs        0.60 μs
ets_lru get                    1897.29 K        0.53 μs  ±4640.15%        0.36 μs        0.79 μs
lru get                         481.94 K        2.07 μs   ±715.47%        1.82 μs        4.57 μs
gen_server_gb_tree_lru get      448.71 K        2.23 μs   ±594.67%        1.97 μs        4.84 μs
gen_server_ets_lru get          447.44 K        2.23 μs   ±755.81%        1.96 μs        4.85 μs

Comparison: 
persistent_term get            2378.55 K
ets_lru get                    1897.29 K - 1.25x slower +0.107 μs
lru get                         481.94 K - 4.94x slower +1.65 μs
gen_server_gb_tree_lru get      448.71 K - 5.30x slower +1.81 μs
gen_server_ets_lru get          447.44 K - 5.32x slower +1.81 μs

##### With input Medium #####
Name                                 ips        average  deviation         median         99th %
persistent_term get            2373.48 K        0.42 μs  ±5865.84%        0.34 μs        0.60 μs
ets_lru get                    1853.90 K        0.54 μs  ±4272.64%        0.36 μs        0.90 μs
lru get                         467.22 K        2.14 μs   ±626.12%        1.84 μs        4.83 μs
gen_server_gb_tree_lru get      458.36 K        2.18 μs   ±739.72%        1.81 μs        6.45 μs
gen_server_ets_lru get          422.36 K        2.37 μs   ±476.74%        2.11 μs        5.01 μs

Comparison: 
persistent_term get            2373.48 K
ets_lru get                    1853.90 K - 1.28x slower +0.118 μs
lru get                         467.22 K - 5.08x slower +1.72 μs
gen_server_gb_tree_lru get      458.36 K - 5.18x slower +1.76 μs
gen_server_ets_lru get          422.36 K - 5.62x slower +1.95 μs

##### With input Large #####
Name                                 ips        average  deviation         median         99th %
persistent_term get            2346.23 K        0.43 μs  ±6076.98%        0.35 μs        0.60 μs
ets_lru get                    1761.58 K        0.57 μs  ±4285.74%        0.37 μs        1.01 μs
gen_server_ets_lru get          431.83 K        2.32 μs   ±577.24%        2.02 μs        5.00 μs
gen_server_gb_tree_lru get      427.93 K        2.34 μs   ±578.70%        1.86 μs        9.31 μs
lru get                         359.07 K        2.78 μs   ±435.38%        1.82 μs       11.47 μs

Comparison: 
persistent_term get            2346.23 K
ets_lru get                    1761.58 K - 1.33x slower +0.141 μs
gen_server_ets_lru get          431.83 K - 5.43x slower +1.89 μs
gen_server_gb_tree_lru get      427.93 K - 5.48x slower +1.91 μs
lru get                         359.07 K - 6.53x slower +2.36 μs

##### With input X-Large #####
Name                                 ips        average  deviation         median         99th %
persistent_term get            2208.57 K        0.45 μs  ±5169.21%        0.36 μs        0.71 μs
ets_lru get                    1531.47 K        0.65 μs  ±3478.19%        0.40 μs        1.51 μs
gen_server_ets_lru get          411.74 K        2.43 μs   ±566.58%        2.07 μs        5.11 μs
gen_server_gb_tree_lru get      393.17 K        2.54 μs   ±626.17%        1.93 μs        6.19 μs
lru get                          96.87 K       10.32 μs   ±230.50%        1.81 μs       99.04 μs

Comparison: 
persistent_term get            2208.57 K
ets_lru get                    1531.47 K - 1.44x slower +0.20 μs
gen_server_ets_lru get          411.74 K - 5.36x slower +1.98 μs
gen_server_gb_tree_lru get      393.17 K - 5.62x slower +2.09 μs
lru get                          96.87 K - 22.80x slower +9.87 μs

##### With input XX-Large #####
Name                                 ips        average  deviation         median         99th %
persistent_term get            1839.15 K        0.54 μs  ±3249.87%        0.46 μs        0.87 μs
ets_lru get                    1149.42 K        0.87 μs  ±2859.72%        0.51 μs        2.38 μs
gen_server_ets_lru get          376.41 K        2.66 μs   ±436.14%        2.23 μs        5.46 μs
gen_server_gb_tree_lru get      338.73 K        2.95 μs  ±1096.40%        2.07 μs        6.97 μs
lru get                           8.60 K      116.34 μs   ±290.55%        2.29 μs     1577.27 μs

Comparison: 
persistent_term get            1839.15 K
ets_lru get                    1149.42 K - 1.60x slower +0.33 μs
gen_server_ets_lru get          376.41 K - 4.89x slower +2.11 μs
gen_server_gb_tree_lru get      338.73 K - 5.43x slower +2.41 μs
lru get                           8.60 K - 213.97x slower +115.80 μs
```

##### Put

```
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-9700KF CPU @ 3.60GHz
Number of Available Cores: 8
Available memory: 31.30 GB
Elixir 1.13.4
Erlang 24.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 5 s
memory time: 0 ns
reduction time: 0 ns
parallel: 4
inputs: Small, Medium, Large, X-Large, XX-Large
Estimated total run time: 2.92 min

Benchmarking ets_lru put with input Small ...
Benchmarking ets_lru put with input Medium ...
Benchmarking ets_lru put with input Large ...
Benchmarking ets_lru put with input X-Large ...
Benchmarking ets_lru put with input XX-Large ...
Benchmarking gen_server_ets_lru get with input Small ...
Benchmarking gen_server_ets_lru get with input Medium ...
Benchmarking gen_server_ets_lru get with input Large ...
Benchmarking gen_server_ets_lru get with input X-Large ...
Benchmarking gen_server_ets_lru get with input XX-Large ...
Benchmarking gen_server_gb_tree_lru put with input Small ...
Benchmarking gen_server_gb_tree_lru put with input Medium ...
Benchmarking gen_server_gb_tree_lru put with input Large ...
Benchmarking gen_server_gb_tree_lru put with input X-Large ...
Benchmarking gen_server_gb_tree_lru put with input XX-Large ...
Benchmarking lru put with input Small ...
Benchmarking lru put with input Medium ...
Benchmarking lru put with input Large ...
Benchmarking lru put with input X-Large ...
Benchmarking lru put with input XX-Large ...
Benchmarking persistent_term put with input Small ...
Benchmarking persistent_term put with input Medium ...
Benchmarking persistent_term put with input Large ...
Benchmarking persistent_term put with input X-Large ...
Benchmarking persistent_term put with input XX-Large ...

##### With input Small #####
Name                                 ips        average  deviation         median         99th %
ets_lru put                    1285.85 K        0.78 μs  ±2785.72%        0.73 μs        1.05 μs
persistent_term put             570.24 K        1.75 μs  ±1019.93%        0.47 μs       23.47 μs
gen_server_ets_lru get          469.34 K        2.13 μs   ±751.14%        1.83 μs        4.90 μs
lru put                         434.45 K        2.30 μs   ±701.66%        2.04 μs        5.07 μs
gen_server_gb_tree_lru put      430.80 K        2.32 μs   ±661.06%        2.00 μs        5.06 μs

Comparison: 
ets_lru put                    1285.85 K
persistent_term put             570.24 K - 2.25x slower +0.98 μs
gen_server_ets_lru get          469.34 K - 2.74x slower +1.35 μs
lru put                         434.45 K - 2.96x slower +1.52 μs
gen_server_gb_tree_lru put      430.80 K - 2.98x slower +1.54 μs

##### With input Medium #####
Name                                 ips        average  deviation         median         99th %
ets_lru put                    1238.03 K        0.81 μs  ±2423.22%        0.76 μs        1.13 μs
persistent_term put             562.93 K        1.78 μs  ±1024.07%        0.48 μs       23.90 μs
gen_server_ets_lru get          470.02 K        2.13 μs   ±755.39%        1.81 μs        4.80 μs
gen_server_gb_tree_lru put      356.08 K        2.81 μs   ±446.58%        2.26 μs       10.49 μs
lru put                         223.24 K        4.48 μs   ±226.03%        4.32 μs        8.87 μs

Comparison: 
ets_lru put                    1238.03 K
persistent_term put             562.93 K - 2.20x slower +0.97 μs
gen_server_ets_lru get          470.02 K - 2.63x slower +1.32 μs
gen_server_gb_tree_lru put      356.08 K - 3.48x slower +2.00 μs
lru put                         223.24 K - 5.55x slower +3.67 μs

##### With input Large #####
Name                                 ips        average  deviation         median         99th %
ets_lru put                    1173.37 K        0.85 μs  ±2635.84%        0.80 μs        1.19 μs
persistent_term put             532.50 K        1.88 μs  ±1019.11%        0.49 μs       25.21 μs
gen_server_ets_lru get          440.29 K        2.27 μs   ±783.60%        1.97 μs        4.98 μs
gen_server_gb_tree_lru put      283.85 K        3.52 μs   ±328.55%        2.50 μs       18.51 μs
lru put                          45.17 K       22.14 μs   ±594.39%       23.28 μs       34.08 μs

Comparison: 
ets_lru put                    1173.37 K
persistent_term put             532.50 K - 2.20x slower +1.03 μs
gen_server_ets_lru get          440.29 K - 2.66x slower +1.42 μs
gen_server_gb_tree_lru put      283.85 K - 4.13x slower +2.67 μs
lru put                          45.17 K - 25.97x slower +21.28 μs

##### With input X-Large #####
Name                                 ips        average  deviation         median         99th %
ets_lru put                    1032.01 K        0.97 μs  ±1992.37%        0.91 μs        1.39 μs
gen_server_ets_lru get          426.28 K        2.35 μs   ±624.16%        1.99 μs        5.09 μs
persistent_term put             361.08 K        2.77 μs   ±525.74%        0.62 μs       28.22 μs
gen_server_gb_tree_lru put      238.30 K        4.20 μs   ±474.72%        2.89 μs       13.30 μs
lru put                           4.70 K      212.63 μs    ±30.44%      231.28 μs      300.35 μs

Comparison: 
ets_lru put                    1032.01 K
gen_server_ets_lru get          426.28 K - 2.42x slower +1.38 μs
persistent_term put             361.08 K - 2.86x slower +1.80 μs
gen_server_gb_tree_lru put      238.30 K - 4.33x slower +3.23 μs
lru put                           4.70 K - 219.44x slower +211.66 μs

##### With input XX-Large #####
Name                                 ips        average  deviation         median         99th %
ets_lru put                     845.48 K        1.18 μs  ±1601.63%        1.12 μs        1.79 μs
gen_server_ets_lru get          374.98 K        2.67 μs   ±489.94%        2.23 μs        5.58 μs
persistent_term put             303.60 K        3.29 μs   ±463.49%        0.70 μs       33.86 μs
gen_server_gb_tree_lru put      164.36 K        6.08 μs  ±1213.71%        3.93 μs       11.59 μs
lru put                           3.45 K      289.60 μs    ±34.55%      289.72 μs      592.56 μs

Comparison: 
ets_lru put                     845.48 K
gen_server_ets_lru get          374.98 K - 2.25x slower +1.48 μs
persistent_term put             303.60 K - 2.78x slower +2.11 μs
gen_server_gb_tree_lru put      164.36 K - 5.14x slower +4.90 μs
lru put                           3.45 K - 244.86x slower +288.42 μs
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

With a very small cache, `lru` outperforms the gen_server implementation here.
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
OldestRank = ets:first(?RANKS),
[{OldestRank, OldestKey}] = ets:take(?RANKS, OldestRank),
ets:delete(?CACHE, OldestKey);
```

The `?RANKS` ets table is an `ordered_set`, so lookup,
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

In the ets-based implementations though, we update the "rank" (a
incrementing integer which identifies the order in which keys were
inserted and accessed):

```erl
update_rank(Rank, Key) ->
    NextRank = next_rank(),
    _ = ets:delete(?RANKS, Rank),
    _ = ets:insert(?RANKS, {NextRank, Key}),
    _ = ets:update_element(?CACHE, Key, {2, NextRank}),
    ok.
```

For the `?CACHE` table which is a `set`, we can say that the
update is roughly constant-time. The `?RANKS` `ordered_set`
table has a logarithmic update time which dominates the other update. This
logarithmic update time scales better than the linear `move_front/2`
implementation.

So we see that the ets-based implementations here are logarithmic for both
`put/2` and `get/2`, while the `lru` implementation is linear on the
`Capacity` of the cache.

### Licence

This work is licensed under the BSD 0-Clause License. See the `LICENSE` file
for more information.
