# LruBench

A benchmark comparison of some LRU cache approaches in Erlang

The caches are implemented in `src/` and a benchmarking script in `bench.exs`
runs the Benchee benchmarks.

<details><summary>The benchmark results...</summary>

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
memory time: 2 s
reduction time: 0 ns
parallel: 1
inputs: Large, Medium, Small, X-Large
Estimated total run time: 2.80 min

Benchmarking ets_lru with input Large ...
Benchmarking ets_lru with input Medium ...
Benchmarking ets_lru with input Small ...
Benchmarking ets_lru with input X-Large ...
Benchmarking gen_server_lru with input Large ...
Benchmarking gen_server_lru with input Medium ...
Benchmarking gen_server_lru with input Small ...
Benchmarking gen_server_lru with input X-Large ...
Benchmarking lru with input Large ...
Benchmarking lru with input Medium ...
Benchmarking lru with input Small ...
Benchmarking lru with input X-Large ...

##### With input Large #####
Name                     ips        average  deviation         median         99th %
ets_lru                28.60       34.97 ms     ±1.66%       34.75 ms       37.28 ms
gen_server_lru         27.25       36.70 ms     ±1.23%       36.56 ms       38.34 ms
lru                    23.76       42.09 ms     ±1.13%       41.97 ms       43.64 ms

Comparison: 
ets_lru                28.60
gen_server_lru         27.25 - 1.05x slower +1.73 ms
lru                    23.76 - 1.20x slower +7.12 ms

Memory usage statistics:

Name              Memory usage
ets_lru                7.01 MB
gen_server_lru         7.01 MB - 1.00x memory usage +0 MB
lru                    5.64 MB - 0.80x memory usage -1.37058 MB

**All measurements for memory usage were the same**

##### With input Medium #####
Name                     ips        average  deviation         median         99th %
ets_lru               289.44        3.45 ms     ±2.51%        3.42 ms        3.78 ms
gen_server_lru        273.22        3.66 ms     ±2.48%        3.63 ms        4.04 ms
lru                   237.54        4.21 ms     ±2.44%        4.17 ms        4.64 ms

Comparison: 
ets_lru               289.44
gen_server_lru        273.22 - 1.06x slower +0.21 ms
lru                   237.54 - 1.22x slower +0.75 ms

Memory usage statistics:

Name              Memory usage
ets_lru              718.23 KB
gen_server_lru       718.23 KB - 1.00x memory usage +0 KB
lru                  576.69 KB - 0.80x memory usage -141.54688 KB

**All measurements for memory usage were the same**

##### With input Small #####
Name                     ips        average  deviation         median         99th %
ets_lru               3.29 K      304.28 μs     ±5.05%      300.17 μs      378.08 μs
lru                   3.26 K      306.54 μs     ±5.01%      302.37 μs      379.54 μs
gen_server_lru        3.19 K      313.90 μs     ±4.60%      310.12 μs      368.25 μs

Comparison: 
ets_lru               3.29 K
lru                   3.26 K - 1.01x slower +2.27 μs
gen_server_lru        3.19 K - 1.03x slower +9.63 μs

Memory usage statistics:

Name                   average  deviation         median         99th %
ets_lru               71.49 KB     ±0.00%       71.49 KB       71.49 KB
lru                   57.83 KB     ±0.11%       57.84 KB       57.84 KB
gen_server_lru        71.49 KB     ±0.01%       71.49 KB       71.49 KB

Comparison: 
ets_lru               71.49 KB
lru                   57.83 KB - 0.81x memory usage -13.66389 KB
gen_server_lru        71.49 KB - 1.00x memory usage -0.00029 KB

##### With input X-Large #####
Name                     ips        average  deviation         median         99th %
ets_lru                 2.83      352.97 ms     ±0.50%      353.18 ms      358.07 ms
gen_server_lru          2.67      374.37 ms     ±1.65%      371.84 ms      389.06 ms
lru                     2.40      416.95 ms     ±0.54%      416.40 ms      423.77 ms

Comparison: 
ets_lru                 2.83
gen_server_lru          2.67 - 1.06x slower +21.41 ms
lru                     2.40 - 1.18x slower +63.98 ms

Memory usage statistics:

Name              Memory usage
ets_lru               70.19 MB
gen_server_lru        70.19 MB - 1.00x memory usage +0 MB
lru                   56.45 MB - 0.80x memory usage -13.73599 MB

**All measurements for memory usage were the same**
```

</details>

### Licence

This work is licensed under the BSD 0-Clause License. See the `LICENSE` file
for more information.
