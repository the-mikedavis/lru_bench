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
Name                     ips        average  deviation         median         99th %
lru                 308.87 K        3.24 μs   ±509.14%        3.07 μs        4.47 μs
gen_server_lru      256.49 K        3.90 μs   ±430.37%        3.65 μs        5.20 μs
ets_lru             244.56 K        4.09 μs   ±414.46%        3.95 μs        5.27 μs

Comparison: 
lru                 308.87 K
gen_server_lru      256.49 K - 1.20x slower +0.66 μs
ets_lru             244.56 K - 1.26x slower +0.85 μs

##### With input Medium #####
Name                     ips        average  deviation         median         99th %
ets_lru             240.28 K        4.16 μs   ±411.54%        4.02 μs        5.40 μs
gen_server_lru      226.69 K        4.41 μs   ±287.84%        3.97 μs       10.08 μs
lru                 189.98 K        5.26 μs   ±175.70%        5.08 μs        9.47 μs

Comparison: 
ets_lru             240.28 K
gen_server_lru      226.69 K - 1.06x slower +0.25 μs
lru                 189.98 K - 1.26x slower +1.10 μs

##### With input Large #####
Name                     ips        average  deviation         median         99th %
ets_lru             234.08 K        4.27 μs   ±291.66%        4.13 μs        5.58 μs
gen_server_lru      204.78 K        4.88 μs   ±276.01%        4.20 μs       16.07 μs
lru                  45.23 K       22.11 μs    ±42.53%       23.17 μs       41.03 μs

Comparison: 
ets_lru             234.08 K
gen_server_lru      204.78 K - 1.14x slower +0.61 μs
lru                  45.23 K - 5.18x slower +17.84 μs

##### With input X-Large #####
Name                     ips        average  deviation         median         99th %
ets_lru             226.97 K        4.41 μs   ±294.63%        4.24 μs        5.74 μs
gen_server_lru      191.14 K        5.23 μs   ±308.39%        4.56 μs       17.23 μs
lru                   4.92 K      203.30 μs    ±40.13%      225.03 μs      296.74 μs

Comparison: 
ets_lru             226.97 K
gen_server_lru      191.14 K - 1.19x slower +0.83 μs
lru                   4.92 K - 46.14x slower +198.89 μs
```

### Analysis

TODO

### Licence

This work is licensed under the BSD 0-Clause License. See the `LICENSE` file
for more information.
