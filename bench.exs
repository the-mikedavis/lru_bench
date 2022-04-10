random_element = fn capacity ->
  max = capacity * 5
  Enum.random(1..max)
end

Benchee.run(
  %{
    "gen_server_lru" => {
      fn {capacity, _pid} ->
        element = random_element.(capacity)
        :gen_server_lru.put(element, element)
        ^element = :gen_server_lru.get(element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :gen_server_lru.start_link(capacity: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    },
    "ets_lru" => {
      fn {capacity, _pid} ->
        element = random_element.(capacity)
        :ets_lru.put(element, element)
        ^element = :ets_lru.get(element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :ets_lru.start_link(capacity: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    },
    "lru" => {
      fn {capacity, lru} ->
        element = random_element.(capacity)
        :lru.add(lru, element, element)
        ^element = :lru.get(lru, element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :lru.start(max_objs: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
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
