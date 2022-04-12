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
persistent_term get        2.64 M        0.38 μs  ±8985.31%        0.28 μs        0.87 μs
ets_lru get                2.26 M        0.44 μs  ±7044.76%        0.28 μs        0.71 μs
lru get                    0.56 M        1.79 μs  ±1301.61%        1.64 μs        2.57 μs
gen_server_lru get         0.52 M        1.91 μs   ±906.87%        1.69 μs        2.86 μs

Comparison: 
persistent_term get        2.64 M
ets_lru get                2.26 M - 1.16x slower +0.0624 μs
lru get                    0.56 M - 4.73x slower +1.42 μs
gen_server_lru get         0.52 M - 5.04x slower +1.53 μs

##### With input Medium #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.66 M        0.38 μs  ±9159.25%        0.28 μs        0.73 μs
ets_lru get                2.20 M        0.45 μs  ±7103.27%        0.28 μs        0.75 μs
lru get                    0.53 M        1.88 μs   ±908.91%        1.66 μs        3.13 μs
gen_server_lru get         0.51 M        1.96 μs   ±922.31%        1.71 μs        3.00 μs

Comparison: 
persistent_term get        2.66 M
ets_lru get                2.20 M - 1.21x slower +0.0782 μs
lru get                    0.53 M - 4.99x slower +1.50 μs
gen_server_lru get         0.51 M - 5.21x slower +1.58 μs

##### With input Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.69 M        0.37 μs  ±9193.19%        0.27 μs        0.86 μs
ets_lru get                2.12 M        0.47 μs  ±6403.44%        0.30 μs        0.84 μs
gen_server_lru get         0.51 M        1.97 μs   ±925.37%        1.71 μs        3.07 μs
lru get                    0.38 M        2.61 μs   ±716.17%        1.69 μs       11.39 μs

Comparison: 
persistent_term get        2.69 M
ets_lru get                2.12 M - 1.27x slower +0.0990 μs
gen_server_lru get         0.51 M - 5.30x slower +1.60 μs
lru get                    0.38 M - 7.00x slower +2.23 μs

##### With input X-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.61 M        0.38 μs  ±9059.27%        0.28 μs        0.86 μs
ets_lru get                1.95 M        0.51 μs  ±6475.90%        0.31 μs        1.09 μs
gen_server_lru get         0.49 M        2.05 μs   ±863.86%        1.73 μs        3.37 μs
lru get                  0.0983 M       10.17 μs   ±238.50%        1.72 μs       98.76 μs

Comparison: 
persistent_term get        2.61 M
ets_lru get                1.95 M - 1.34x slower +0.130 μs
gen_server_lru get         0.49 M - 5.34x slower +1.66 μs
lru get                  0.0983 M - 26.54x slower +9.79 μs

##### With input XX-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term get        2.22 M        0.45 μs  ±7920.59%        0.33 μs        0.92 μs
ets_lru get                1.47 M        0.68 μs  ±4887.24%        0.41 μs        1.72 μs
gen_server_lru get         0.45 M        2.23 μs   ±842.16%        1.84 μs        3.92 μs
lru get                  0.0105 M       94.90 μs   ±273.15%        1.98 μs     1149.43 μs

Comparison: 
persistent_term get        2.22 M
ets_lru get                1.47 M - 1.51x slower +0.23 μs
gen_server_lru get         0.45 M - 4.95x slower +1.78 μs
lru get                  0.0105 M - 210.71x slower +94.45 μs
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
persistent_term put        1.81 M        0.55 μs  ±6172.41%        0.37 μs        0.62 μs
ets_lru put                1.34 M        0.75 μs  ±3816.46%        0.71 μs        0.99 μs
lru put                    0.46 M        2.15 μs   ±975.05%        1.97 μs        3.49 μs
gen_server_lru put         0.42 M        2.37 μs   ±596.54%        2.35 μs        3.26 μs

Comparison: 
persistent_term put        1.81 M
ets_lru put                1.34 M - 1.35x slower +0.192 μs
lru put                    0.46 M - 3.89x slower +1.60 μs
gen_server_lru put         0.42 M - 4.29x slower +1.82 μs

##### With input Medium #####
Name                          ips        average  deviation         median         99th %
persistent_term put        1.79 M        0.56 μs  ±6190.79%        0.37 μs        0.63 μs
ets_lru put                1.31 M        0.76 μs  ±4001.57%        0.73 μs        1.02 μs
gen_server_lru put         0.41 M        2.43 μs   ±589.96%        2.41 μs        3.36 μs
lru put                    0.24 M        4.24 μs   ±344.13%        4.05 μs        8.42 μs

Comparison: 
persistent_term put        1.79 M
ets_lru put                1.31 M - 1.37x slower +0.20 μs
gen_server_lru put         0.41 M - 4.37x slower +1.88 μs
lru put                    0.24 M - 7.60x slower +3.68 μs

##### With input Large #####
Name                          ips        average  deviation         median         99th %
persistent_term put        1.76 M        0.57 μs  ±6039.95%        0.38 μs        0.65 μs
ets_lru put                1.24 M        0.80 μs  ±2746.59%        0.79 μs        1.19 μs
gen_server_lru put         0.40 M        2.48 μs   ±576.14%        2.47 μs        3.46 μs
lru put                  0.0464 M       21.57 μs    ±42.21%       22.85 μs       37.46 μs

Comparison: 
persistent_term put        1.76 M
ets_lru put                1.24 M - 1.41x slower +0.24 μs
gen_server_lru put         0.40 M - 4.36x slower +1.91 μs
lru put                  0.0464 M - 37.92x slower +21.01 μs

##### With input X-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term put        1.69 M        0.59 μs  ±4176.24%        0.40 μs        0.81 μs
ets_lru put                1.19 M        0.84 μs  ±2308.70%        0.82 μs        1.15 μs
gen_server_lru put         0.39 M        2.59 μs   ±578.76%        2.56 μs        3.63 μs
lru put                 0.00479 M      208.79 μs    ±30.56%      224.60 μs      301.90 μs

Comparison: 
persistent_term put        1.69 M
ets_lru put                1.19 M - 1.41x slower +0.24 μs
gen_server_lru put         0.39 M - 4.36x slower +1.99 μs
lru put                 0.00479 M - 352.05x slower +208.19 μs

##### With input XX-Large #####
Name                          ips        average  deviation         median         99th %
persistent_term put     1374.75 K        0.73 μs  ±3607.47%        0.51 μs        1.04 μs
ets_lru put              988.72 K        1.01 μs  ±2482.24%        0.96 μs        1.43 μs
gen_server_lru put       349.47 K        2.86 μs   ±531.04%        2.84 μs        4.07 μs
lru put                    2.86 K      349.14 μs    ±40.55%      346.82 μs      742.76 μs

Comparison: 
persistent_term put     1374.75 K
ets_lru put              988.72 K - 1.39x slower +0.28 μs
gen_server_lru put       349.47 K - 3.93x slower +2.13 μs
lru put                    2.86 K - 479.98x slower +348.41 μs
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
