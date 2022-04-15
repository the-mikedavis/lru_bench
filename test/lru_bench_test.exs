defmodule LruBenchTest do
  use ExUnit.Case

  test "gen_server_ets_lru correctness" do
    {:ok, cache} = :gen_server_ets_lru.start_link(capacity: 2)

    :gen_server_ets_lru.put(cache, 1, 1)
    assert :gen_server_ets_lru.get(cache, 1, :error) == 1
    :gen_server_ets_lru.put(cache, 2, 2)
    :gen_server_ets_lru.put(cache, 3, 3)
    assert :gen_server_ets_lru.get(cache, 2, :error) == 2
    assert :gen_server_ets_lru.get(cache, 3, :error) == 3

    # evicted
    assert :gen_server_ets_lru.get(cache, 1, :error) == :error

    assert :gen_server_ets_lru.get(cache, 2, :error) == 2
    :gen_server_ets_lru.put(cache, 4, 4)
    assert :gen_server_ets_lru.get(cache, 4, :error) == 4

    # evicted because 2 was more recently accessed
    assert :gen_server_ets_lru.get(cache, 3, :error) == :error
  end

  test "ets_lru correctness" do
    cache = :ets_lru.init(2)

    :ets_lru.put(cache, 1, 1)
    assert :ets_lru.get(cache, 1, :error) == 1
    :ets_lru.put(cache, 2, 2)
    :ets_lru.put(cache, 3, 3)
    assert :ets_lru.get(cache, 2, :error) == 2
    assert :ets_lru.get(cache, 3, :error) == 3

    # evicted
    assert :ets_lru.get(cache, 1, :error) == :error

    assert :ets_lru.get(cache, 2, :error) == 2
    :ets_lru.put(cache, 4, 4)
    assert :ets_lru.get(cache, 4, :error) == 4

    # evicted because 2 was more recently accessed
    assert :ets_lru.get(cache, 3, :error) == :error
  end
end
