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
time: 10 s
memory time: 0 ns
reduction time: 0 ns
parallel: 1
inputs: Small, Medium, Large, X-Large, XX-Large
Estimated total run time: 4 min

##### With input Small #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.63 M        0.38 μs  ±9078.70%        0.28 μs        0.86 μs
ets_lru get                2.06 M        0.48 μs  ±5957.45%        0.31 μs        0.79 μs
lru get                    0.56 M        1.80 μs  ±1038.89%        1.68 μs        2.57 μs
gen_server_lru get         0.51 M        1.98 μs  ±1032.39%        1.76 μs        2.88 μs

Comparison: 
persistent_term get        2.63 M
ets_lru get                2.06 M - 1.28x slower +0.105 μs
lru get                    0.56 M - 4.74x slower +1.42 μs
gen_server_lru get         0.51 M - 5.22x slower +1.60 μs

##### With input Medium #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.69 M        0.37 μs  ±9134.18%        0.28 μs        0.86 μs
ets_lru get                2.03 M        0.49 μs  ±7545.79%        0.31 μs        0.82 μs
lru get                    0.53 M        1.89 μs   ±994.46%        1.70 μs        3.20 μs
gen_server_lru get         0.50 M        2.00 μs  ±1038.34%        1.77 μs        2.95 μs

Comparison: 
persistent_term get        2.69 M
ets_lru get                2.03 M - 1.32x slower +0.120 μs
lru get                    0.53 M - 5.08x slower +1.52 μs
gen_server_lru get         0.50 M - 5.36x slower +1.62 μs

##### With input Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.65 M        0.38 μs  ±9114.09%        0.28 μs        0.87 μs
ets_lru get                2.00 M        0.50 μs  ±6075.28%        0.32 μs        0.88 μs
gen_server_lru get         0.49 M        2.02 μs  ±1016.37%        1.78 μs        3.02 μs
lru get                    0.39 M        2.57 μs   ±557.24%        1.73 μs       10.98 μs

Comparison: 
persistent_term get        2.65 M
ets_lru get                2.00 M - 1.33x slower +0.124 μs
gen_server_lru get         0.49 M - 5.37x slower +1.65 μs
lru get                    0.39 M - 6.83x slower +2.20 μs

##### With input X-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.65 M        0.38 μs  ±8999.44%        0.28 μs        0.85 μs
ets_lru get                1.88 M        0.53 μs  ±6405.08%        0.33 μs        1.04 μs
gen_server_lru get         0.48 M        2.07 μs  ±1018.43%        1.80 μs        3.17 μs
lru get                   0.101 M        9.91 μs   ±238.65%        1.77 μs       95.55 μs

Comparison: 
persistent_term get        2.65 M
ets_lru get                1.88 M - 1.41x slower +0.156 μs
gen_server_lru get         0.48 M - 5.49x slower +1.69 μs
lru get                   0.101 M - 26.23x slower +9.53 μs

##### With input XX-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.24 M        0.45 μs  ±7999.65%        0.33 μs        0.94 μs
ets_lru get                1.52 M        0.66 μs  ±4063.58%        0.39 μs        1.62 μs
gen_server_lru get         0.45 M        2.23 μs   ±964.52%        1.87 μs        3.66 μs
lru get                  0.0109 M       91.73 μs   ±273.68%        2.03 μs     1115.39 μs

Comparison: 
persistent_term get        2.24 M
ets_lru get                1.52 M - 1.47x slower +0.21 μs
gen_server_lru get         0.45 M - 4.99x slower +1.78 μs
lru get                  0.0109 M - 205.30x slower +91.29 μs
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
inputs: Small, Medium, Large, X-Large, XX-Large
Estimated total run time: 4 min

##### With input Small #####
Name                          ips        average  deviation         median         99th %
persistent_term put     1835.63 K        0.54 μs  ±6130.51%        0.36 μs        0.62 μs
ets_lru put              942.64 K        1.06 μs  ±2564.19%        1.09 μs        1.54 μs
lru put                  476.88 K        2.10 μs   ±941.44%        1.95 μs        3.32 μs
gen_server_lru put       396.20 K        2.52 μs   ±632.55%        2.48 μs        3.90 μs

Comparison: 
persistent_term put     1835.63 K
ets_lru put              942.64 K - 1.95x slower +0.52 μs
lru put                  476.88 K - 3.85x slower +1.55 μs
gen_server_lru put       396.20 K - 4.63x slower +1.98 μs

##### With input Medium #####
Name                          ips        average  deviation         median         99th %
persistent_term put     1835.97 K        0.54 μs  ±6138.86%        0.37 μs        0.64 μs
ets_lru put              926.23 K        1.08 μs  ±2514.11%        1.12 μs        1.58 μs
gen_server_lru put       387.02 K        2.58 μs   ±625.88%        2.54 μs        4.04 μs
lru put                  246.15 K        4.06 μs   ±331.42%        3.94 μs        7.86 μs

Comparison: 
persistent_term put     1835.97 K
ets_lru put              926.23 K - 1.98x slower +0.53 μs
gen_server_lru put       387.02 K - 4.74x slower +2.04 μs
lru put                  246.15 K - 7.46x slower +3.52 μs

##### With input Large #####
Name                          ips        average  deviation         median         99th %
persistent_term put     1794.62 K        0.56 μs  ±6023.81%        0.38 μs        0.65 μs
ets_lru put              898.09 K        1.11 μs  ±1632.55%        1.17 μs        1.62 μs
gen_server_lru put       377.62 K        2.65 μs   ±616.45%        2.60 μs        4.16 μs
lru put                   47.71 K       20.96 μs    ±41.12%       22.18 μs       39.40 μs

Comparison: 
persistent_term put     1794.62 K
ets_lru put              898.09 K - 2.00x slower +0.56 μs
gen_server_lru put       377.62 K - 4.75x slower +2.09 μs
lru put                   47.71 K - 37.61x slower +20.40 μs

##### With input X-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term put     1720.14 K        0.58 μs  ±5864.63%        0.39 μs        0.76 μs
ets_lru put              885.90 K        1.13 μs  ±1982.63%        1.18 μs        1.53 μs
gen_server_lru put       374.03 K        2.67 μs   ±617.88%        2.64 μs        3.75 μs
lru put                    4.89 K      204.63 μs    ±30.42%      219.37 μs      298.21 μs

Comparison: 
persistent_term put     1720.14 K
ets_lru put              885.90 K - 1.94x slower +0.55 μs
gen_server_lru put       374.03 K - 4.60x slower +2.09 μs
lru put                    4.89 K - 351.99x slower +204.05 μs

##### With input XX-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term put     1411.10 K        0.71 μs  ±3664.18%        0.51 μs        1.02 μs
ets_lru put              764.51 K        1.31 μs  ±1508.30%        1.33 μs        1.83 μs
gen_server_lru put       347.04 K        2.88 μs   ±594.07%        2.84 μs        4.09 μs
lru put                    2.88 K      347.63 μs    ±41.18%      341.88 μs      748.53 μs

Comparison: 
persistent_term put     1411.10 K
ets_lru put              764.51 K - 1.85x slower +0.60 μs
gen_server_lru put       347.04 K - 4.07x slower +2.17 μs
lru put                    2.88 K - 490.54x slower +346.92 μs
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
