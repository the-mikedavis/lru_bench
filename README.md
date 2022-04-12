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
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-9700KF CPU @ 3.60GHz
Number of Available Cores: 8
Available memory: 31.30 GB
Elixir 1.13.3
Erlang 24.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 5 s
memory time: 0 ns
reduction time: 0 ns
parallel: 4
inputs: Small, Medium, Large, X-Large, XX-Large
Estimated total run time: 2.33 min

##### With input Small #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.56 M        0.39 μs  ±6324.02%        0.31 μs        0.57 μs
ets_lru get                2.06 M        0.49 μs  ±5199.57%        0.32 μs        0.70 μs
lru get                    0.48 M        2.08 μs   ±702.26%        1.84 μs        4.67 μs
gen_server_lru get         0.44 M        2.28 μs   ±653.31%        2.00 μs        5.09 μs

Comparison: 
persistent_term get        2.56 M
ets_lru get                2.06 M - 1.24x slower +0.0945 μs
lru get                    0.48 M - 5.31x slower +1.69 μs
gen_server_lru get         0.44 M - 5.84x slower +1.89 μs

##### With input Medium #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.54 M        0.39 μs  ±6221.10%        0.31 μs        0.57 μs
ets_lru get                2.01 M        0.50 μs  ±5236.49%        0.32 μs        0.80 μs
lru get                    0.45 M        2.23 μs   ±563.24%        1.95 μs        4.92 μs
gen_server_lru get         0.45 M        2.24 μs   ±694.30%        1.95 μs        4.88 μs

Comparison: 
persistent_term get        2.54 M
ets_lru get                2.01 M - 1.27x slower +0.105 μs
lru get                    0.45 M - 5.66x slower +1.83 μs
gen_server_lru get         0.45 M - 5.70x slower +1.85 μs

##### With input Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.45 M        0.41 μs  ±5756.45%        0.33 μs        0.58 μs
ets_lru get                1.90 M        0.53 μs  ±4984.33%        0.34 μs        0.92 μs
gen_server_lru get         0.42 M        2.36 μs   ±485.79%        2.06 μs        5.06 μs
lru get                    0.34 M        2.90 μs   ±394.84%        1.94 μs       12.09 μs

Comparison: 
persistent_term get        2.45 M
ets_lru get                1.90 M - 1.29x slower +0.118 μs
gen_server_lru get         0.42 M - 5.77x slower +1.95 μs
lru get                    0.34 M - 7.10x slower +2.49 μs

##### With input X-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.39 M        0.42 μs  ±5780.88%        0.33 μs        0.65 μs
ets_lru get                1.71 M        0.58 μs  ±3801.29%        0.36 μs        1.34 μs
gen_server_lru get         0.41 M        2.45 μs   ±423.45%        2.09 μs        5.30 μs
lru get                  0.0955 M       10.47 μs   ±230.29%        1.82 μs      101.87 μs

Comparison: 
persistent_term get        2.39 M
ets_lru get                1.71 M - 1.40x slower +0.165 μs
gen_server_lru get         0.41 M - 5.87x slower +2.03 μs
lru get                  0.0955 M - 25.05x slower +10.05 μs

##### With input XX-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        1.98 M        0.51 μs  ±3412.39%        0.42 μs        0.83 μs
ets_lru get                1.29 M        0.77 μs  ±2985.80%        0.46 μs        2.01 μs
gen_server_lru get         0.38 M        2.61 μs   ±399.39%        2.18 μs        5.55 μs
lru get                 0.00909 M      109.99 μs   ±282.54%        2.25 μs     1457.59 μs

Comparison: 
persistent_term get        1.98 M
ets_lru get                1.29 M - 1.53x slower +0.27 μs
gen_server_lru get         0.38 M - 5.17x slower +2.11 μs
lru get                 0.00909 M - 217.59x slower +109.48 μs
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
time: 5 s
memory time: 0 ns
reduction time: 0 ns
parallel: 4
inputs: Small, Medium, Large, X-Large, XX-Large
Estimated total run time: 2.33 min

##### With input Small #####
Name                          ips        average  deviation         median         99th %
ets_lru put             1213.03 K        0.82 μs  ±2347.63%        0.78 μs        1.10 μs
persistent_term put      550.16 K        1.82 μs  ±1000.19%        0.50 μs       24.56 μs
lru put                  397.59 K        2.52 μs   ±430.50%        2.25 μs        5.37 μs
gen_server_lru put       358.64 K        2.79 μs   ±397.13%        2.65 μs        5.77 μs

Comparison: 
ets_lru put             1213.03 K
persistent_term put      550.16 K - 2.20x slower +0.99 μs
lru put                  397.59 K - 3.05x slower +1.69 μs
gen_server_lru put       358.64 K - 3.38x slower +1.96 μs

##### With input Medium #####
Name                          ips        average  deviation         median         99th %
ets_lru put             1151.49 K        0.87 μs  ±2414.58%        0.82 μs        1.20 μs
persistent_term put      543.91 K        1.84 μs  ±1004.08%        0.50 μs       25.51 μs
gen_server_lru put       346.04 K        2.89 μs   ±363.82%        2.75 μs        5.96 μs
lru put                  223.79 K        4.47 μs   ±203.98%        4.28 μs        9.04 μs

Comparison: 
ets_lru put             1151.49 K
persistent_term put      543.91 K - 2.12x slower +0.97 μs
gen_server_lru put       346.04 K - 3.33x slower +2.02 μs
lru put                  223.79 K - 5.15x slower +3.60 μs

##### With input Large #####
Name                          ips        average  deviation         median         99th %
ets_lru put             1102.91 K        0.91 μs  ±2298.23%        0.86 μs        1.29 μs
persistent_term put      526.72 K        1.90 μs   ±982.11%        0.50 μs       25.52 μs
gen_server_lru put       321.93 K        3.11 μs   ±364.70%        2.92 μs        6.29 μs
lru put                   46.25 K       21.62 μs    ±41.20%       22.91 μs       38.79 μs

Comparison: 
ets_lru put             1102.91 K
persistent_term put      526.72 K - 2.09x slower +0.99 μs
gen_server_lru put       321.93 K - 3.43x slower +2.20 μs
lru put                   46.25 K - 23.85x slower +20.72 μs

##### With input X-Large #####
Name                          ips        average  deviation         median         99th %
ets_lru put             1006.04 K        0.99 μs  ±2263.18%        0.94 μs        1.39 μs
persistent_term put      392.12 K        2.55 μs   ±559.97%        0.60 μs       25.34 μs
gen_server_lru put       298.15 K        3.35 μs   ±369.70%        3.17 μs        6.58 μs
lru put                    4.78 K      209.39 μs    ±30.60%      224.79 μs      307.47 μs

Comparison: 
ets_lru put             1006.04 K
persistent_term put      392.12 K - 2.57x slower +1.56 μs
gen_server_lru put       298.15 K - 3.37x slower +2.36 μs
lru put                    4.78 K - 210.65x slower +208.39 μs

##### With input XX-Large #####
Name                          ips        average  deviation         median         99th %
ets_lru put              835.50 K        1.20 μs  ±1576.50%        1.13 μs        1.76 μs
persistent_term put      311.88 K        3.21 μs   ±503.50%        0.71 μs       33.03 μs
gen_server_lru put       278.36 K        3.59 μs   ±247.46%        3.50 μs        6.74 μs
lru put                    3.41 K      293.09 μs    ±35.36%      290.61 μs      586.28 μs

Comparison: 
ets_lru put              835.50 K
persistent_term put      311.88 K - 2.68x slower +2.01 μs
gen_server_lru put       278.36 K - 3.00x slower +2.40 μs
lru put                    3.41 K - 244.88x slower +291.89 μs
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
