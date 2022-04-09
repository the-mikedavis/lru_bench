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
ets_lru                26.62       37.57 ms     ±2.07%       37.38 ms       40.16 ms
gen_server_lru         26.19       38.18 ms     ±1.04%       38.07 ms       39.87 ms
lru                    23.61       42.36 ms     ±1.00%       42.24 ms       44.17 ms

Comparison: 
ets_lru                26.62
gen_server_lru         26.19 - 1.02x slower +0.61 ms
lru                    23.61 - 1.13x slower +4.79 ms

Memory usage statistics:

Name              Memory usage
ets_lru                7.01 MB
gen_server_lru         7.01 MB - 1.00x memory usage +0 MB
lru                    5.64 MB - 0.80x memory usage -1.37058 MB

**All measurements for memory usage were the same**

##### With input Medium #####
Name                     ips        average  deviation         median         99th %
ets_lru               268.24        3.73 ms     ±2.38%        3.72 ms        4.04 ms
gen_server_lru        262.75        3.81 ms     ±2.04%        3.79 ms        4.08 ms
lru                   232.82        4.30 ms     ±2.40%        4.27 ms        4.69 ms

Comparison: 
ets_lru               268.24
gen_server_lru        262.75 - 1.02x slower +0.0780 ms
lru                   232.82 - 1.15x slower +0.57 ms

Memory usage statistics:

Name              Memory usage
ets_lru              718.23 KB
gen_server_lru       718.23 KB - 1.00x memory usage +0 KB
lru                  576.69 KB - 0.80x memory usage -141.54688 KB

**All measurements for memory usage were the same**

##### With input Small #####
Name                     ips        average  deviation         median         99th %
lru                   3.12 K      320.17 μs     ±4.51%      317.30 μs      378.81 μs
gen_server_lru        3.09 K      324.01 μs     ±4.87%      320.59 μs      402.10 μs
ets_lru               3.06 K      326.41 μs     ±6.07%      321.31 μs      421.47 μs

Comparison: 
lru                   3.12 K
gen_server_lru        3.09 K - 1.01x slower +3.85 μs
ets_lru               3.06 K - 1.02x slower +6.24 μs

Memory usage statistics:

Name                   average  deviation         median         99th %
lru                   57.83 KB     ±0.11%       57.84 KB       57.84 KB
gen_server_lru        71.49 KB     ±0.01%       71.49 KB       71.49 KB
ets_lru               71.49 KB     ±0.01%       71.49 KB       71.49 KB

Comparison: 
lru                   57.84 KB
gen_server_lru        71.49 KB - 1.24x memory usage +13.66 KB
ets_lru               71.49 KB - 1.24x memory usage +13.66 KB

##### With input X-Large #####
Name                     ips        average  deviation         median         99th %
ets_lru                 2.66      376.28 ms     ±1.29%      374.69 ms      386.61 ms
gen_server_lru          2.60      385.28 ms     ±0.51%      384.88 ms      388.91 ms
lru                     2.30      434.12 ms     ±1.64%      431.64 ms      450.82 ms

Comparison: 
ets_lru                 2.66
gen_server_lru          2.60 - 1.02x slower +9.00 ms
lru                     2.30 - 1.15x slower +57.85 ms

Memory usage statistics:

Name              Memory usage
ets_lru               70.19 MB
gen_server_lru        70.19 MB - 1.00x memory usage +0 MB
lru                   56.45 MB - 0.80x memory usage -13.73599 MB

**All measurements for memory usage were the same**
```
