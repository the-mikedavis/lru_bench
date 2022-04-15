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

  def gen_server_ets_lru_get do
    {
      fn {capacity, pid} ->
        element = random_element(capacity)
        :gen_server_ets_lru.get(pid, element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, cache} = :gen_server_ets_lru.start_link(capacity: capacity)

        capacity
        |> random_elements()
        |> Enum.each(&:gen_server_ets_lru.put(cache, &1, &1))

        {capacity, cache}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def gen_server_ets_lru_put do
    {
      fn {capacity, cache} ->
        element = random_element(capacity)
        :gen_server_ets_lru.put(cache, element, element)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :gen_server_ets_lru.start_link(capacity: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def gen_server_gb_tree_lru_get do
    {
      fn {capacity, pid} ->
        element = random_element(capacity)
        :gen_server_gb_tree_lru.get(pid, element, :error)
      end,
      before_scenario: fn capacity ->
        {:ok, cache} = :gen_server_gb_tree_lru.start_link(capacity: capacity)

        capacity
        |> random_elements()
        |> Enum.each(&:gen_server_gb_tree_lru.put(cache, &1, &1))

        {capacity, cache}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def gen_server_gb_tree_lru_put do
    {
      fn {capacity, cache} ->
        element = random_element(capacity)
        :gen_server_gb_tree_lru.put(cache, element, element)
      end,
      before_scenario: fn capacity ->
        {:ok, pid} = :gen_server_gb_tree_lru.start_link(capacity: capacity)
        {capacity, pid}
      end,
      after_scenario: fn {_capacity, pid} -> GenServer.stop(pid) end
    }
  end

  def ets_lru_get do
    {
      fn {capacity, cache} ->
        element = random_element(capacity)
        :ets_lru.get(cache, element, :error)
      end,
      before_scenario: fn capacity ->
        cache = :ets_lru.init(capacity)

        capacity
        |> random_elements()
        |> Enum.each(&:ets_lru.put(cache, &1, &1))

        {capacity, cache}
      end,
      after_scenario: fn {_capacity, cache} ->
        :ets_lru.clear(cache)
      end
    }
  end

  def ets_lru_put do
    {
      fn {capacity, cache} ->
        element = random_element(capacity)
        :ets_lru.put(cache, element, element)
      end,
      before_scenario: fn capacity ->
        {capacity, :ets_lru.init(capacity)}
      end,
      after_scenario: fn {_capacity, cache} -> :ets_lru.clear(cache) end
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

  def persistent_term_get do
    {
      fn capacity ->
        element = random_element(capacity)
        :persistent_term.get({__MODULE__, element}, :undefined)
      end,
      before_scenario: fn capacity ->
        capacity
        |> random_elements()
        |> Enum.each(&:persistent_term.put({__MODULE__, &1}, &1))

        capacity
      end
    }
  end

  def persistent_term_put do
    fn capacity ->
      element = random_element(capacity)
      :persistent_term.put({__MODULE__, element}, element)
    end
  end

  def settings do
    [
      inputs: [
        {"Small", 10},
        {"Medium", 100},
        {"Large", 1_000},
        {"X-Large", 10_000},
        {"XX-Large", 100_000}
      ],
      time: 5,
      parallel: 4
    ]
  end

  def bench_get do
    Benchee.run(
      %{
        "gen_server_gb_tree_lru get" => gen_server_gb_tree_lru_get(),
        "gen_server_ets_lru get" => gen_server_ets_lru_get(),
        "ets_lru get" => ets_lru_get(),
        "lru get" => lru_get(),
        "persistent_term get" => persistent_term_get()
      },
      settings()
    )
  end

  def bench_put do
    Benchee.run(
      %{
        "gen_server_gb_tree_lru put" => gen_server_gb_tree_lru_put(),
        "gen_server_ets_lru get" => gen_server_ets_lru_get(),
        "ets_lru put" => ets_lru_put(),
        "lru put" => lru_put(),
        "persistent_term put" => persistent_term_put()
      },
      settings()
    )
  end
end
