defmodule LruBench.MixProject do
  use Mix.Project

  def project do
    [
      app: :lru_bench,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:benchee, "~> 1.1"},
      {:lru, "~> 2.4"}
    ]
  end
end
