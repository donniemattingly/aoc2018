defmodule Aoc12 do
  @moduledoc """
  Documentation for Aoc12.
  """

  
  def input(filename) do
    case File.read(filename) do
      {:ok, body} -> body
      {:error, reason} -> :error
    end
  end

  def string_pattern_to_bitstring(pattern_string) do
    pattern_string
    |> String.split("", trim: true)
    |> Enum.map(fn char -> 
      case char do
        "#" -> <<1>>
        "." -> <<0>>
      end
    end)
    |> Enum.reduce(fn x, acc -> acc <> x end)
  end

  def parse_add_or_remove_rule(rule_string) do
    [pattern | [result | _]] = rule_string 
    |> String.split("=>") 
    |> Enum.map(&String.trim/1)

    bit_pattern = string_pattern_to_bitstring(pattern)

    case result do
      "." -> {:remove, bit_pattern}
      "#" -> {:add, bit_pattern}
    end
  end

  def parsed_input do
    [initial | rules] = 
      input("../../inputs/input-12.0.txt")
      |> String.split("\n")
      |> Enum.filter(&String.first/1)

    initial_parsed = ".." <> (initial 
      |> String.split(":") 
      |> List.last() 
      |> String.trim())

    rules_parsed = rules
    |> Enum.map(fn(rule) -> 
      rule 
      |> String.split("=>") 
      |> List.first() 
      |> String.trim()
    end)

    {initial_parsed, rules_parsed}
  end

  def build_new_generation("", acc, rules) do
    acc
  end

  def build_new_generation(remaining, acc, rules) when byte_size(remaining) < 5 do
    test_str = remaining <> String.duplicate(".", 5-byte_size(remaining))
    new_acc = acc <> case should_have_pot(test_str, rules) do
                       true -> "#"
                       false -> "."
                     end
    new_remaining = binary_part(remaining, 1, byte_size(remaining) - 1)
    build_new_generation(new_remaining, new_acc, rules)
  end
  
  def build_new_generation(remaining, acc, rules) do
    IO.inspect({remaining, acc})
    test_str = binary_part(remaining, 0, 5)
    new_acc = acc <> case should_have_pot(test_str, rules) do
                       true -> "#"
                       false -> "."
                     end
    new_remaining = binary_part(remaining, 1, byte_size(remaining) - 1)
    build_new_generation(new_remaining, new_acc, rules)
  end

  def next_generation(state, rules, zero_cursor) do
    first = ".." <> binary_part(state, 0, 3) |> should_have_pot(rules)
    second = "." <> binary_part(state, 0, 4) |> should_have_pot(rules)
    
    {res, zero_cursor} = case true do
                           ^first -> {build_new_generation(".." <> state, "", rules), zero_cursor + 2}
                           ^second -> {build_new_generation("." <> state, "", rules), zero_cursor + 1}
                           _ -> {build_new_generation(state, "", rules), zero_cursor}
                         end
    {res, zero_cursor}
  end


  def gen(old_state, old_age, add_rules, sub_rules, current_pot) do
    
  end

  def run_generations(initial, rules, generations) do
    1..generations
    |> Enum.reduce({initial, 0}, fn _, {state, zero_cursor} -> 
      next_generation(state, rules, zero_cursor)
    end)
  end

  def plant_status(str, add, sub) do
    add_plant = add |>
      Enum.any?(fn (rule) -> 
        case rule do
          ^str -> true
          _ -> false
        end
      end)
    
    remove_plant = sub |>
      Enum.any?(fn (rule) -> 
        case rule do
          ^str -> true
          _ -> false
        end
      end)

    {add_plant, remove_plant}
  end

  def next_state(state, rules) do
    {add, sub} = rules
    
    add
  end
end




