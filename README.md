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
Elixir 1.13.4
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
Name                             ips        average  deviation         median         99th %
persistent_term get           2.38 M        0.42 μs  ±5427.92%        0.34 μs        0.61 μs
ets_lru get                   1.94 M        0.51 μs  ±4468.01%        0.35 μs        0.78 μs
lru get                       0.47 M        2.15 μs   ±691.26%        1.90 μs        4.71 μs
gen_server_ets_lru get        0.45 M        2.23 μs   ±589.32%        1.96 μs        4.95 μs

Comparison: 
persistent_term get           2.38 M
ets_lru get                   1.94 M - 1.22x slower +0.0942 μs
lru get                       0.47 M - 5.11x slower +1.73 μs
gen_server_ets_lru get        0.45 M - 5.31x slower +1.81 μs

##### With input Medium #####
Name                             ips        average  deviation         median         99th %
persistent_term get           2.38 M        0.42 μs  ±5647.24%        0.34 μs        0.60 μs
ets_lru get                   1.91 M        0.52 μs  ±4593.03%        0.35 μs        0.85 μs
lru get                       0.48 M        2.07 μs   ±726.13%        1.77 μs        4.69 μs
gen_server_ets_lru get        0.45 M        2.24 μs   ±715.22%        1.92 μs        4.97 μs

Comparison: 
persistent_term get           2.38 M
ets_lru get                   1.91 M - 1.25x slower +0.104 μs
lru get                       0.48 M - 4.92x slower +1.65 μs
gen_server_ets_lru get        0.45 M - 5.33x slower +1.82 μs

##### With input Large #####
Name                             ips        average  deviation         median         99th %
persistent_term get           2.37 M        0.42 μs  ±5927.56%        0.34 μs        0.61 μs
ets_lru get                   1.81 M        0.55 μs  ±4466.07%        0.36 μs        0.95 μs
gen_server_ets_lru get        0.40 M        2.47 μs   ±505.20%        2.19 μs        5.20 μs
lru get                       0.36 M        2.75 μs   ±447.04%        1.78 μs       11.38 μs

Comparison: 
persistent_term get           2.37 M
ets_lru get                   1.81 M - 1.31x slower +0.129 μs
gen_server_ets_lru get        0.40 M - 5.85x slower +2.05 μs
lru get                       0.36 M - 6.50x slower +2.32 μs

##### With input X-Large #####
Name                             ips        average  deviation         median         99th %
persistent_term get           2.21 M        0.45 μs  ±5076.66%        0.36 μs        0.72 μs
ets_lru get                   1.60 M        0.63 μs  ±3454.00%        0.38 μs        1.43 μs
gen_server_ets_lru get        0.41 M        2.46 μs   ±470.07%        2.09 μs        5.31 μs
lru get                     0.0965 M       10.36 μs   ±231.50%        1.85 μs       98.92 μs

Comparison: 
persistent_term get           2.21 M
ets_lru get                   1.60 M - 1.38x slower +0.173 μs
gen_server_ets_lru get        0.41 M - 5.43x slower +2.01 μs
lru get                     0.0965 M - 22.90x slower +9.91 μs

##### With input XX-Large #####
Name                             ips        average  deviation         median         99th %
persistent_term get           1.83 M        0.55 μs  ±3012.21%        0.46 μs        0.88 μs
ets_lru get                   1.23 M        0.82 μs  ±2450.19%        0.49 μs        2.13 μs
gen_server_ets_lru get        0.34 M        2.93 μs   ±374.38%        2.50 μs        5.97 μs
lru get                    0.00869 M      115.12 μs   ±291.48%        2.27 μs     1553.78 μs

Comparison: 
persistent_term get           1.83 M
ets_lru get                   1.23 M - 1.49x slower +0.27 μs
gen_server_ets_lru get        0.34 M - 5.37x slower +2.39 μs
lru get                    0.00869 M - 210.69x slower +114.58 μs
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
Estimated total run time: 2.33 min

##### With input Small #####
Name                             ips        average  deviation         median         99th %
ets_lru put                1205.12 K        0.83 μs  ±2689.29%        0.78 μs        1.14 μs
persistent_term put         530.69 K        1.88 μs   ±984.44%        0.50 μs       23.32 μs
lru put                     408.95 K        2.45 μs   ±478.05%        2.19 μs        5.34 μs
gen_server_ets_lru put      353.55 K        2.83 μs   ±411.29%        2.67 μs        5.88 μs

Comparison: 
ets_lru put                1205.12 K
persistent_term put         530.69 K - 2.27x slower +1.05 μs
lru put                     408.95 K - 2.95x slower +1.62 μs
gen_server_ets_lru put      353.55 K - 3.41x slower +2.00 μs

##### With input Medium #####
Name                             ips        average  deviation         median         99th %
ets_lru put                1170.27 K        0.85 μs  ±2596.62%        0.81 μs        1.16 μs
persistent_term put         535.98 K        1.87 μs  ±1028.33%        0.50 μs       23.85 μs
gen_server_ets_lru put      344.25 K        2.90 μs   ±404.82%        2.74 μs        5.95 μs
lru put                     218.10 K        4.59 μs   ±215.24%        4.40 μs        9.05 μs

Comparison: 
ets_lru put                1170.27 K
persistent_term put         535.98 K - 2.18x slower +1.01 μs
gen_server_ets_lru put      344.25 K - 3.40x slower +2.05 μs
lru put                     218.10 K - 5.37x slower +3.73 μs

##### With input Large #####
Name                             ips        average  deviation         median         99th %
ets_lru put                1111.57 K        0.90 μs  ±2386.61%        0.86 μs        1.26 μs
persistent_term put         510.64 K        1.96 μs   ±964.72%        0.51 μs       25.51 μs
gen_server_ets_lru put      317.00 K        3.15 μs   ±431.09%        2.92 μs        6.46 μs
lru put                      46.02 K       21.73 μs    ±39.93%       23.19 μs       30.45 μs

Comparison: 
ets_lru put                1111.57 K
persistent_term put         510.64 K - 2.18x slower +1.06 μs
gen_server_ets_lru put      317.00 K - 3.51x slower +2.25 μs
lru put                      46.02 K - 24.15x slower +20.83 μs

##### With input X-Large #####
Name                             ips        average  deviation         median         99th %
ets_lru put                 981.65 K        1.02 μs  ±2039.82%        0.96 μs        1.44 μs
persistent_term put         338.06 K        2.96 μs   ±519.81%        0.68 μs       28.44 μs
gen_server_ets_lru put      285.95 K        3.50 μs   ±305.85%        3.33 μs        6.84 μs
lru put                       4.71 K      212.23 μs    ±30.35%      230.68 μs      297.28 μs

Comparison: 
ets_lru put                 981.65 K
persistent_term put         338.06 K - 2.90x slower +1.94 μs
gen_server_ets_lru put      285.95 K - 3.43x slower +2.48 μs
lru put                       4.71 K - 208.34x slower +211.22 μs

##### With input XX-Large #####
Name                             ips        average  deviation         median         99th %
ets_lru put                 821.28 K        1.22 μs  ±1451.44%        1.16 μs        1.82 μs
persistent_term put         310.61 K        3.22 μs   ±495.01%        0.70 μs       36.35 μs
gen_server_ets_lru put      271.22 K        3.69 μs   ±273.01%        3.58 μs        6.75 μs
lru put                       3.43 K      291.50 μs    ±34.86%      288.14 μs      595.28 μs

Comparison: 
ets_lru put                 821.28 K
persistent_term put         310.61 K - 2.64x slower +2.00 μs
gen_server_ets_lru put      271.22 K - 3.03x slower +2.47 μs
lru put                       3.43 K - 239.40x slower +290.28 μs
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
