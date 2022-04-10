random_element = fn capacity ->
  max = capacity * 5
  Enum.random(1..max)
end

elements = fn capacity ->
  Stream.repeatedly(fn -> random_element.(capacity) end)
  |> Enum.take(capacity)
end

Benchee.run(
  %{
    "gen_server_lru get" => {
      fn {capacity, _pid} ->
        element = random_element.(capacity)
        :gen_server_lru.get(element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :gen_server_lru.start_link(capacity: capacity)
        Enum.each(elements.(capacity), &:gen_server_lru.put(&1, &1))
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    },
    "gen_server_lru put" => {
      fn {capacity, _pid} ->
        element = random_element.(capacity)
        :gen_server_lru.put(element, element)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :gen_server_lru.start_link(capacity: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    },
    "lru get" => {
      fn {capacity, lru} ->
        element = random_element.(capacity)
        :lru.get(lru, element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :lru.start(max_objs: capacity)
        Enum.each(elements.(capacity), &:lru.add(pid, &1, &1))
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    },
    "lru put" => {
      fn {capacity, lru} ->
        element = random_element.(capacity)
        :lru.add(lru, element, element)
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
