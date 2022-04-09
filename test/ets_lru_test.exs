defmodule EtsLruTest do
  use ExUnit.Case

  test "ets_lru correctness test" do
    :ets_lru.start_link(capacity: 2)

    :ets_lru.put(1, 1)
    assert :ets_lru.get(1, :error) == 1
    :ets_lru.put(2, 2)
    :ets_lru.put(3, 3)
    assert :ets_lru.get(2, :error) == 2
    assert :ets_lru.get(3, :error) == 3

    # evicted
    assert :ets_lru.get(1, :error) == :error

    assert :ets_lru.get(2, :error) == 2
    :ets_lru.put(4, 4)
    assert :ets_lru.get(4, :error) == 4

    # evicted because 2 was more recently accessed
    assert :ets_lru.get(3, :error) == :error
  end
end
