open Batteries

type timestamp = 
  { year: int;
    month: int;
    day: int;
    hour: int;
    min: int;
  }

type guard_action = Begin of int
                  | Sleep 
                  | Wake

type guard_record = 
  { time : timestamp;
    action: guard_action;
    id: int;
  }

type guard_stats = 
  { total: int;
    min: int;
  }

let get_id v = 
  match v with 
  | Begin i -> i
  | _ -> -1

(* from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let timeint_of_string str =
  let digits_only = String.filter Char.is_digit str in
  int_of_string digits_only

let calc_timestamp record_string =
  let timestamp_part = List.hd (String.split_on_char ']' record_string) in
  let split_timestamp = String.split_on_char ' ' timestamp_part in
  let date_parts = String.split_on_char '-' (List.hd split_timestamp) in
  let time_parts = String.split_on_char ':' (List.nth split_timestamp 1) in
  let t = List.map timeint_of_string (List.append date_parts time_parts) in
  { year = List.nth t 0;
    month = List.nth t 1;
    day = List.nth t 2;
    hour = List.nth t 3;
    min = List.nth t 4;
  }

let begin_action_of_string str = 
  let guard_number = int_of_string (String.filter Char.is_digit str) in
  Begin guard_number

let action_of_string = function
  | "wakes up" -> Wake
  | "falls asleep" -> Sleep
  | o -> begin_action_of_string o

let get_action_of_record_string str =
  let action_string =  String.strip ( List.nth (String.split_on_char ']' str) 1) in
  action_of_string action_string
  
let parse_record record_string  =
  { action = get_action_of_record_string record_string;
    time = calc_timestamp record_string;
    id = -1;
  }

let parsed_input filename = 
  let lines = read_lines filename in
  List.map parse_record lines

let compare_record g1 g2 = 
  let y = compare g1.time.year g2.time.year in
  let mo = compare g1.time.month g2.time.month in
  let d = compare g1.time.day g2.time.day in
  let h = compare g1.time.hour g2.time.hour in
  let mi = compare g1.time.min g2.time.min in
  if y != 0 then y else
    if mo != 0 then mo else
      if d != 0 then d else
        if h != 0 then h else mi

let identify_accumulator acc value =
  let val_id = get_id value.action in
  let id = if val_id > 0 then val_id else fst acc in
  let cum_records = snd acc in
  let new_record = { time = value.time; action = value.action; id = id; } in
  (id, new_record :: cum_records)

let identify_all_records records =
  let sorted_records = List.sort compare_record records in
  let final = List.fold_left identify_accumulator (-1, []) sorted_records in
  List.rev (snd final)
  
let get_min_between t1 t2 =
  (43200 * (t2.month - t1.month)) + (1440 * (t2.day - t1.day)) + (60 * (t2.hour - t1.hour)) + (t2.min - t1.min)

let get_initial_minute_arr ()  = 
  Array.make 60 0

let inc_array_at arr i =
  let cur = Array.get arr i in
  Array.set arr i (cur + 1)
  

let update_min_array arr duration start_time = 
  let end_time = start_time + duration in
  let indices = map (fun x -> x mod 60) (start_time -- (end_time - 1)) in
  BatEnum.fold (fun acc x -> inc_array_at acc x; acc) arr indices

let guard_stats_accumulator acc value = 
  let (previous, stats, minutes) = acc in 
  match (previous.action, value.action) with
    | (Sleep, Wake) -> let duration = get_min_between previous.time value.time in
                       let start_min = previous.time.min in
                       let updated = update_min_array minutes duration start_min in
                       Printf.printf "id: %i from: %i to %i\n" value.id previous.time.min value.time.min;
                       Printf.printf "duration: %i\n" duration;
                       Printf.printf "start_time: %i\n" start_min;
                       (value, { total = stats.total + duration;
                         min = stats.min;
                         },
                        minutes)
    | _ -> (value, stats, minutes)
                                    
let calc_guard_stats guard_records =
  List.fold_left guard_stats_accumulator (List.hd guard_records, {total = 0; min = 0;}, get_initial_minute_arr ()) (List.tl guard_records)

let get_guard_stats records = 
  let ids = List.sort_uniq compare (List.map (fun x -> x.id) records) in
  let filtered_records = (List.map (fun x -> (x, List.filter (fun y -> y.id = x) records)) ids) in
  let stats = List.map (fun x -> (fst x, calc_guard_stats (snd x))) filtered_records in
  stats

let get_total x =
  let (r, stats, minutes) = (snd x) in
  stats.total

let most_time_asleep stats =
  let sorted = List.sort (fun x y -> compare (get_total x) (get_total y)) stats in
  List.hd (List.rev sorted)

(*let get_most_common_min_asleep_for_guard arr =
  let indexed = Array.to_list (Array.mapi (fun i x -> (i, x)) arr) in
  List.hd (List.rev (List.sort (fun x y -> (compare (fst x) > (fst y))) indexed)) *)                       

let max_freq value  = 
  let (id, x) = value in
  let (r, stats, arr) = x in 
  let m = Array.max arr in
  let tup = List.find (fun (x,y) -> y = m) (Array.to_list (Array.mapi (fun i x -> (i, x)) arr)) in
  (id, m, tup)
                            

let guard_with_most_freq_asleep_min stats = 
  let m = List.map max_freq stats in
  m
    (*
Examples of strings
[1518-09-09 00:57] wakes up
[1518-04-22 00:30] falls asleep
[1518-08-30 00:00] Guard #1279 begins shift
     *)
