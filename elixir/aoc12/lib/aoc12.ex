defmodule Aoc12 do
  @moduledoc """
  Documentation for Aoc12.
  """
  
  @test_input "../../inputs/input-12.0.txt"
  @real_input "../../inputs/input-12.1.txt"
  
  def input(filename) do
    case File.read(filename) do
      {:ok, body} -> body
      {:error, reason} -> :error
    end
  end

  def parse_add_or_remove_rule(rule_string) do
    [pattern | [result | _]] = rule_string 
    |> String.split("=>") 
    |> Enum.map(&String.trim/1)

    case result do
      "." -> {:remove, pattern}
      "#" -> {:add, pattern}
    end
  end

  def parsed_input(filename) do
    [initial | rules] = 
      input(filename)
      |> String.split("\n")
      |> Enum.filter(&String.first/1)


    initial_parsed = initial 
      |> String.split(":") 
      |> List.last() 
      |> String.trim()
      |> pad_state

    rules_parsed = rules
    |> Enum.map(&parse_add_or_remove_rule/1)

    {initial_parsed, rules_parsed}
  end


  # current is a length 5 substring of the state
  def grow(current, rules) do
    result = rules 
    |> Enum.find(fn {_, rule} ->
      case rule do
        ^current -> true
        _ -> false
      end
    end)

    case result do
      {action, _ } -> action
      nil -> :maintain
    end
  end

  def prefix_state(state) do
    case binary_part(state, 0, 3) do
      "..."<> _  -> {state, 0}
      ".." <> _ -> {"." <> state, 1}
      "." <> _ -> {".." <> state, 2}
      _ -> {"..." <> state, 3}
    end
  end

  def suffix_state(state) do
    suffix = binary_part(state, byte_size(state) - 2, 2)
    case suffix do
      "..."<> _  -> state
      ".." <> _ -> state <> "."
      "." <> _ -> state <> ".."
      _ ->  state <> "..."
    end
  end

  def pad_state(state) do
    state
    |> suffix_state
    |> prefix_state
  end

  def wrap_results(results) do
    m = [:maintain, :maintain]
    m ++ results ++ m
  end

  def apply_results_to_state(results, state) do
    state 
    |> String.split("", trim: true)
    |> Enum.zip(results)
    |> Enum.map(fn {c, action} -> 
      case action do
        :add -> "#"
        _ -> "."
      end
    end)
    |> Enum.join("")
  end

  # state should always be padded heref
  def generation(state, rules, zero_cursor) do
    new_state = 2..byte_size(state)-3 
    |> Enum.map(fn x -> binary_part(state, x - 2, 5) end)
    |> Enum.map(&grow(&1,rules))
    |> wrap_results
    |> apply_results_to_state(state)
    
    {trimmed_state, trimmed_cursor} = trim_and_update_cursor(new_state, zero_cursor)
    {padded_state, pad_cursor} = pad_state(trimmed_state)

    {padded_state, trimmed_cursor+pad_cursor }
  end

  def run_generations(state, rules, zero_cursor, count) do
    1..count
    |> Enum.reduce({state, zero_cursor}, fn (gen, {old_state, zc}) -> 
      generation(old_state, rules, zc)
    end)
  end

  def run_generations2(state, rules, zero_cursor, count) do
    1..count
    |> Enum.reduce({state, zero_cursor, get_len_plant_region(state), 0}, fn (gen, acc={old_state, zc, region_size, score_diff}) -> 
      {new_state, new_cursor} = generation(old_state, rules, zc)
      old_score = get_score(old_state, zc)
      new_score = get_score(new_state, new_cursor)
      {new_state, new_cursor, get_len_plant_region(new_state), new_score-old_score}
    end)
  end

  def get_score(state, cursor) do
    0..byte_size(state)
    |> Enum.map(fn x -> x - cursor end)
    |> Enum.zip(String.split(state, "", trim: true))
    |> Enum.filter(fn {idx, char} -> char == "#" end)
    |> Enum.map(&elem(&1,0))
    |> Enum.sum
  end

  def trim_and_update_cursor(state, cursor) do
    new_state = state |> String.trim_leading(".")
    {new_state, cursor - (byte_size(state) - byte_size(new_state))}
  end

  def get_len_plant_region(state) do
    state
    |> String.trim(".")
    |> byte_size
  end

  def part_one() do
    {{state, zero_cursor}, rules} = parsed_input(@real_input)
    generation_count = 20
    {end_state, end_cursor} = run_generations(state, rules, zero_cursor, generation_count)
    get_score(end_state, end_cursor)
  end

  def part_two() do
    {{state, zero_cursor}, rules} = parsed_input(@real_input)
    generation_count = 140
    {end_state, end_cursor, region_size, score_diff} = run_generations2(state, rules, zero_cursor, generation_count)
    final_score = get_score(end_state, end_cursor)
    result = (50_000_000_000 - 2000) * score_diff + final_score
    IO.puts(result)

    result
  end

  def measure(function) do
    function
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end

end




