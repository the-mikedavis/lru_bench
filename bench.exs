fill = fn table, size ->
  :ets.insert(table, Enum.map(1..size, &{&1, &1}))
end


# Operating System: Linux
# CPU Information: Intel(R) Core(TM) i7-9700KF CPU @ 3.60GHz
# Number of Available Cores: 8
# Available memory: 31.30 GB
# Elixir 1.13.3
# Erlang 24.2
# 
# Benchmark suite executing with the following configuration:
# warmup: 2 s
# time: 10 s
# memory time: 0 ns
# reduction time: 0 ns
# parallel: 1
# inputs: Large, Medium, Small, X-Large
# Estimated total run time: 48 s
# 
# Benchmarking ets:info(Tab, size) with input Large ...
# Benchmarking ets:info(Tab, size) with input Medium ...
# Benchmarking ets:info(Tab, size) with input Small ...
# Benchmarking ets:info(Tab, size) with input X-Large ...
# 
# ##### With input Large #####
# Name                          ips        average  deviation         median         99th %
# ets:info(Tab, size)       10.38 M       96.30 ns ±15272.68%          91 ns         116 ns
# 
# ##### With input Medium #####
# Name                          ips        average  deviation         median         99th %
# ets:info(Tab, size)       10.45 M       95.66 ns ±15427.20%          91 ns         116 ns
# 
# ##### With input Small #####
# Name                          ips        average  deviation         median         99th %
# ets:info(Tab, size)       10.44 M       95.77 ns ±15213.21%          90 ns         115 ns
# 
# ##### With input X-Large #####
# Name                          ips        average  deviation         median         99th %
# ets:info(Tab, size)       10.49 M       95.37 ns ±15210.00%          90 ns         116 ns

Benchee.run(
  %{
    "ets:info(Tab, size)" => {
      fn table -> :ets.info(table, :size) end,
      before_scenario: fn size ->
        table = :ets.new(:foo, [:private, :ordered_set])
        fill.(table, size)
        table
      end
    }
  },
  inputs: %{
    "Small" => 10,
    "Medium" => 100,
    "Large" => 1_000,
    "X-Large" => 10_000
  },
  time: 10
)
