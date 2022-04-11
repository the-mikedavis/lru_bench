defmodule LruBench do
  @moduledoc false

  def random_element(capacity) do
    max = capacity * 5
    Enum.random(1..max)
  end

  def random_elements(capacity) do
    Stream.repeatedly(fn -> random_element(capacity) end)
    |> Enum.take(capacity)
  end

  def gen_server_lru_get do
    {
      fn {capacity, _pid} ->
        element = random_element(capacity)
        :gen_server_lru.get(element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :gen_server_lru.start_link(capacity: capacity)

        capacity
        |> random_elements()
        |> Enum.each(&:gen_server_lru.put(&1, &1))

        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def gen_server_lru_put do
    {
      fn {capacity, _pid} ->
        element = random_element(capacity)
        :gen_server_lru.put(element, element)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :gen_server_lru.start_link(capacity: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def ets_lru_get do
    {
      fn capacity ->
        element = random_element(capacity)
        :ets_lru.get(element, :error)
      end,
      before_scenario: fn capacity ->
        :ets_lru.init(capacity)

        capacity
        |> random_elements()
        |> Enum.each(&:ets_lru.put(&1, &1))

        capacity
      end,
      after_scenario: fn _ -> :ets_lru.clear() end
    }
  end

  def ets_lru_put do
    {
      fn capacity ->
        element = random_element(capacity)
        :ets_lru.put(element, element)
      end,
      before_scenario: fn capacity ->
        :ets_lru.init(capacity)
        capacity
      end,
      after_scenario: fn _ -> :ets_lru.clear() end
    }
  end

  def lru_get do
    {
      fn {capacity, lru} ->
        element = random_element(capacity)
        :lru.get(lru, element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :lru.start(max_objs: capacity)

        capacity
        |> random_elements()
        |> Enum.each(&:lru.add(pid, &1, &1))

        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def lru_put do
    {
      fn {capacity, lru} ->
        element = random_element(capacity)
        :lru.add(lru, element, element)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :lru.start(max_objs: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def settings do
    [
      inputs: [
        {"Small", 10},
        {"Medium", 100},
        {"Large", 1_000},
        {"X-Large", 10_000}
      ],
      time: 10
    ]
  end

  def bench_get do
    Benchee.run(
      %{
        "gen_server_lru get" => gen_server_lru_get(),
        "ets_lru get" => ets_lru_get(),
        "lru get" => lru_get()
      },
      settings()
    )
  end

  def bench_put do
    Benchee.run(
      %{
        "gen_server_lru put" => gen_server_lru_put(),
        "ets_lru put" => ets_lru_put(),
        "lru put" => lru_put()
      },
      settings()
    )
  end
end
