# LruBench

A benchmark comparison of some LRU cache approaches in Erlang

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
ets_lru2               29.06       34.41 ms     ±1.13%       34.39 ms       35.70 ms
ets_lru                27.71       36.09 ms     ±0.94%       36.04 ms       37.08 ms
gen_server_lru         27.47       36.40 ms     ±1.13%       36.36 ms       37.97 ms
lru                    23.70       42.19 ms     ±2.08%       41.91 ms       45.76 ms

Comparison: 
ets_lru2               29.06
ets_lru                27.71 - 1.05x slower +1.68 ms
gen_server_lru         27.47 - 1.06x slower +1.99 ms
lru                    23.70 - 1.23x slower +7.79 ms

Memory usage statistics:

Name              Memory usage
ets_lru2               7.01 MB
ets_lru                7.01 MB - 1.00x memory usage +0 MB
gen_server_lru         7.01 MB - 1.00x memory usage +0 MB
lru                    5.64 MB - 0.80x memory usage -1.37058 MB

**All measurements for memory usage were the same**

##### With input Medium #####
Name                     ips        average  deviation         median         99th %
ets_lru2              291.43        3.43 ms     ±1.60%        3.43 ms        3.62 ms
ets_lru               277.05        3.61 ms     ±1.46%        3.60 ms        3.77 ms
gen_server_lru        273.82        3.65 ms     ±2.12%        3.64 ms        3.94 ms
lru                   229.89        4.35 ms     ±2.76%        4.31 ms        4.84 ms

Comparison: 
ets_lru2              291.43
ets_lru               277.05 - 1.05x slower +0.178 ms
gen_server_lru        273.82 - 1.06x slower +0.22 ms
lru                   229.89 - 1.27x slower +0.92 ms

Memory usage statistics:

Name              Memory usage
ets_lru2             718.23 KB
ets_lru              718.23 KB - 1.00x memory usage +0 KB
gen_server_lru       718.23 KB - 1.00x memory usage +0 KB
lru                  576.69 KB - 0.80x memory usage -141.54688 KB

**All measurements for memory usage were the same**

##### With input Small #####
Name                     ips        average  deviation         median         99th %
ets_lru2              3.29 K      304.39 μs     ±3.69%      300.81 μs      343.94 μs
ets_lru               3.18 K      314.87 μs     ±3.55%      311.93 μs      354.50 μs
lru                   3.13 K      319.83 μs     ±4.62%      316.58 μs      379.90 μs
gen_server_lru        3.10 K      322.73 μs     ±5.64%      319.34 μs      397.13 μs

Comparison: 
ets_lru2              3.29 K
ets_lru               3.18 K - 1.03x slower +10.48 μs
lru                   3.13 K - 1.05x slower +15.44 μs
gen_server_lru        3.10 K - 1.06x slower +18.34 μs

Memory usage statistics:

Name                   average  deviation         median         99th %
ets_lru2              71.49 KB     ±0.00%       71.49 KB       71.49 KB
ets_lru               71.49 KB     ±0.00%       71.49 KB       71.49 KB
lru                   57.83 KB     ±0.12%       57.84 KB       57.84 KB
gen_server_lru        71.49 KB     ±0.00%       71.49 KB       71.49 KB

Comparison: 
ets_lru2              71.49 KB
ets_lru               71.49 KB - 1.00x memory usage -0.00008 KB
lru                   57.83 KB - 0.81x memory usage -13.66634 KB
gen_server_lru        71.49 KB - 1.00x memory usage -0.00004 KB

##### With input X-Large #####
Name                     ips        average  deviation         median         99th %
ets_lru2                2.88      346.66 ms     ±0.33%      346.89 ms      349.42 ms
ets_lru                 2.74      365.16 ms     ±0.49%      365.43 ms      368.87 ms
gen_server_lru          2.68      373.27 ms     ±1.00%      372.18 ms      384.18 ms
lru                     2.32      431.53 ms     ±1.21%      430.65 ms      444.58 ms

Comparison: 
ets_lru2                2.88
ets_lru                 2.74 - 1.05x slower +18.51 ms
gen_server_lru          2.68 - 1.08x slower +26.62 ms
lru                     2.32 - 1.24x slower +84.87 ms

Memory usage statistics:

Name              Memory usage
ets_lru2              70.19 MB
ets_lru               70.19 MB - 1.00x memory usage +0 MB
gen_server_lru        70.19 MB - 1.00x memory usage +0 MB
lru                   56.45 MB - 0.80x memory usage -13.73599 MB

**All measurements for memory usage were the same**
```
