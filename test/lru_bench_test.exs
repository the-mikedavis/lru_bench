defmodule LruBenchTest do
  use ExUnit.Case
  doctest LruBench

  test "greets the world" do
    assert LruBench.hello() == :world
  end
end
