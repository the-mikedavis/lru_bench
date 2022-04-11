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
ets_lru get            2182.11 K        0.46 μs  ±7242.39%        0.29 μs        0.74 μs
lru get                 564.83 K        1.77 μs   ±940.84%        1.67 μs        2.49 μs
gen_server_lru get      500.17 K        2.00 μs   ±829.78%        1.77 μs        2.88 μs

Comparison: 
ets_lru get            2182.11 K
lru get                 564.83 K - 3.86x slower +1.31 μs
gen_server_lru get      500.17 K - 4.36x slower +1.54 μs

##### With input Medium #####
Name                         ips        average  deviation         median         99th %
ets_lru get            2077.18 K        0.48 μs  ±6749.11%        0.31 μs        0.82 μs
lru get                 537.27 K        1.86 μs   ±887.50%        1.68 μs        3.15 μs
gen_server_lru get      493.09 K        2.03 μs   ±833.53%        1.79 μs        3.00 μs

Comparison: 
ets_lru get            2077.18 K
lru get                 537.27 K - 3.87x slower +1.38 μs
gen_server_lru get      493.09 K - 4.21x slower +1.55 μs

##### With input Large #####
Name                         ips        average  deviation         median         99th %
ets_lru get            1999.97 K        0.50 μs  ±5402.73%        0.32 μs        0.92 μs
gen_server_lru get      487.96 K        2.05 μs   ±815.00%        1.80 μs        3.08 μs
lru get                 389.14 K        2.57 μs   ±556.13%        1.70 μs       11.27 μs

Comparison: 
ets_lru get            1999.97 K
gen_server_lru get      487.96 K - 4.10x slower +1.55 μs
lru get                 389.14 K - 5.14x slower +2.07 μs

##### With input X-Large #####
Name                         ips        average  deviation         median         99th %
ets_lru get            1871.99 K        0.53 μs  ±6278.42%        0.33 μs        1.06 μs
gen_server_lru get      466.36 K        2.14 μs   ±800.43%        1.84 μs        3.41 μs
lru get                  97.24 K       10.28 μs   ±240.39%        1.78 μs       99.87 μs

Comparison: 
ets_lru get            1871.99 K
gen_server_lru get      466.36 K - 4.01x slower +1.61 μs
lru get                  97.24 K - 19.25x slower +9.75 μs
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

##### With input Small #####
Name                         ips        average  deviation         median         99th %
ets_lru put             936.69 K        1.07 μs  ±2300.63%        1.09 μs        1.39 μs
lru put                 490.40 K        2.04 μs   ±693.42%        1.89 μs        3.42 μs
gen_server_lru put      408.99 K        2.45 μs   ±687.27%        2.39 μs        3.35 μs

Comparison: 
ets_lru put             936.69 K
lru put                 490.40 K - 1.91x slower +0.97 μs
gen_server_lru put      408.99 K - 2.29x slower +1.38 μs

##### With input Medium #####
Name                         ips        average  deviation         median         99th %
ets_lru put             910.52 K        1.10 μs  ±2093.23%        1.13 μs        1.43 μs
gen_server_lru put      400.87 K        2.49 μs   ±667.99%        2.44 μs        3.50 μs
lru put                 247.37 K        4.04 μs   ±308.55%        3.90 μs        8.11 μs

Comparison: 
ets_lru put             910.52 K
gen_server_lru put      400.87 K - 2.27x slower +1.40 μs
lru put                 247.37 K - 3.68x slower +2.94 μs

##### With input Large #####
Name                         ips        average  deviation         median         99th %
ets_lru put             871.97 K        1.15 μs  ±1898.55%        1.19 μs        1.60 μs
gen_server_lru put      392.58 K        2.55 μs   ±655.40%        2.51 μs        3.61 μs
lru put                  47.34 K       21.12 μs    ±41.38%       22.41 μs       33.41 μs

Comparison: 
ets_lru put             871.97 K
gen_server_lru put      392.58 K - 2.22x slower +1.40 μs
lru put                  47.34 K - 18.42x slower +19.98 μs

##### With input X-Large #####
Name                         ips        average  deviation         median         99th %
ets_lru put             844.93 K        1.18 μs  ±2104.85%        1.22 μs        1.56 μs
gen_server_lru put      373.37 K        2.68 μs   ±648.62%        2.61 μs        3.82 μs
lru put                   4.78 K      209.09 μs    ±30.75%      224.63 μs      309.65 μs

Comparison: 
ets_lru put             844.93 K
gen_server_lru put      373.37 K - 2.26x slower +1.49 μs
lru put                   4.78 K - 176.66x slower +207.90 μs
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
