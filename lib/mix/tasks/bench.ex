defmodule Mix.Tasks.Bench do
  @moduledoc "Runs a benchmark"

  def run(["get"]) do
    LruBench.bench_get()
  end

  def run(["put"]) do
    LruBench.bench_put()
  end

  def run(_) do
    Mix.shell().error("Usage: mix bench {get|put}")
  end
end
