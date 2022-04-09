# LruBench

A benchmark comparison of some LRU cache approaches in Erlang

```
Compiling 1 file (.erl)
Generated lru_bench app
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
Estimated total run time: 3.73 min

Benchmarking ets_lru with input Large ...
Benchmarking ets_lru with input Medium ...
Benchmarking ets_lru with input Small ...
Benchmarking ets_lru with input X-Large ...
Benchmarking ets_lru2 with input Large ...
Benchmarking ets_lru2 with input Medium ...
Benchmarking ets_lru2 with input Small ...
Benchmarking ets_lru2 with input X-Large ...
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
ets_lru2               26.88       37.20 ms     ±1.08%       37.11 ms       38.63 ms
ets_lru                26.51       37.72 ms     ±2.14%       37.50 ms       40.47 ms
gen_server_lru         26.43       37.84 ms     ±1.14%       37.77 ms       39.53 ms
lru                    23.89       41.86 ms     ±0.99%       41.75 ms       43.45 ms

Comparison: 
ets_lru2               26.88
ets_lru                26.51 - 1.01x slower +0.52 ms
gen_server_lru         26.43 - 1.02x slower +0.64 ms
lru                    23.89 - 1.13x slower +4.66 ms

Memory usage statistics:

Name              Memory usage
ets_lru2               7.01 MB
ets_lru                7.01 MB - 1.00x memory usage +0 MB
gen_server_lru         7.01 MB - 1.00x memory usage +0 MB
lru                    5.64 MB - 0.80x memory usage -1.37058 MB

**All measurements for memory usage were the same**

##### With input Medium #####
Name                     ips        average  deviation         median         99th %
ets_lru2              274.40        3.64 ms     ±1.71%        3.64 ms        3.85 ms
ets_lru               264.50        3.78 ms     ±3.37%        3.75 ms        4.34 ms
gen_server_lru        260.97        3.83 ms     ±2.15%        3.82 ms        4.09 ms
lru                   237.77        4.21 ms     ±1.65%        4.20 ms        4.44 ms

Comparison: 
ets_lru2              274.40
ets_lru               264.50 - 1.04x slower +0.136 ms
gen_server_lru        260.97 - 1.05x slower +0.187 ms
lru                   237.77 - 1.15x slower +0.56 ms

Memory usage statistics:

Name              Memory usage
ets_lru2             718.23 KB
ets_lru              718.23 KB - 1.00x memory usage +0 KB
gen_server_lru       718.23 KB - 1.00x memory usage +0 KB
lru                  576.69 KB - 0.80x memory usage -141.54688 KB

**All measurements for memory usage were the same**

##### With input Small #####
Name                     ips        average  deviation         median         99th %
lru                   3.19 K      313.39 μs     ±3.07%      311.54 μs      346.76 μs
ets_lru2              3.13 K      319.94 μs     ±3.53%      316.74 μs      359.74 μs
ets_lru               3.07 K      325.79 μs     ±4.64%      321.94 μs      394.37 μs
gen_server_lru        3.07 K      325.97 μs     ±4.28%      322.77 μs      374.17 μs

Comparison: 
lru                   3.19 K
ets_lru2              3.13 K - 1.02x slower +6.54 μs
ets_lru               3.07 K - 1.04x slower +12.40 μs
gen_server_lru        3.07 K - 1.04x slower +12.58 μs

Memory usage statistics:

Name                   average  deviation         median         99th %
lru                   57.83 KB     ±0.10%       57.84 KB       57.84 KB
ets_lru2              71.49 KB     ±0.00%       71.49 KB       71.49 KB
ets_lru               71.49 KB     ±0.01%       71.49 KB       71.49 KB
gen_server_lru        71.49 KB     ±0.01%       71.49 KB       71.49 KB

Comparison: 
lru                   57.84 KB
ets_lru2              71.49 KB - 1.24x memory usage +13.66 KB
ets_lru               71.49 KB - 1.24x memory usage +13.66 KB
gen_server_lru        71.49 KB - 1.24x memory usage +13.66 KB

##### With input X-Large #####
Name                     ips        average  deviation         median         99th %
ets_lru                 2.72      367.25 ms     ±0.64%      366.47 ms      371.48 ms
ets_lru2                2.72      367.51 ms     ±0.50%      367.24 ms      372.43 ms
gen_server_lru          2.62      381.34 ms     ±0.63%      380.54 ms      387.08 ms
lru                     2.38      419.68 ms     ±0.31%      419.62 ms      422.06 ms

Comparison: 
ets_lru                 2.72
ets_lru2                2.72 - 1.00x slower +0.26 ms
gen_server_lru          2.62 - 1.04x slower +14.09 ms
lru                     2.38 - 1.14x slower +52.43 ms

Memory usage statistics:

Name              Memory usage
ets_lru               70.19 MB
ets_lru2              70.19 MB - 1.00x memory usage +0 MB
gen_server_lru        70.19 MB - 1.00x memory usage +0 MB
lru                   56.45 MB - 0.80x memory usage -13.73599 MB

**All measurements for memory usage were the same**
```
