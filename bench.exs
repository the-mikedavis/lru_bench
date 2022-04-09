capacity = 100
:gen_server_lru.start_link(capacity: capacity)
:ets_lru.start_link(capacity: capacity)
{:ok, lru} = :lru.start(max_objs: capacity)

variance = 1..250
elements = Stream.repeatedly(fn -> Enum.random(variance) end)

Benchee.run(
  %{
    "gen_server_lru" => fn input ->
       Enum.each(input, fn element ->
         :ok = :gen_server_lru.put(element, element)
         ^element = :gen_server_lru.get(element, :error)
       end)
     end,
    "ets_lru" => fn input ->
       Enum.each(input, fn element ->
         :ok = :ets_lru.put(element, element)
         ^element = :ets_lru.get(element, :error)
       end)
     end,
    "lru" => fn input ->
       Enum.each(input, fn element ->
         :lru.add(lru, element, element)
         ^element = :lru.get(lru, element, :error)
       end)
     end,
  },
  inputs: %{
    "Small" => elements |> Enum.take(100),
    "Medium" => elements |> Enum.take(1_000),
    "Large" => elements |> Enum.take(10_000),
    "X-Large" => elements |> Enum.take(100_000),
  },
  time: 10,
  memory_time: 2
)
