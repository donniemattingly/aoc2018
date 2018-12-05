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
  snd final
            

let rec sublist b e l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail

let get_begin_records records =
  let paired = List.mapi (fun i x -> (x, i)) records in
  let begins = List.filter (fun x -> let record = fst x in 
                        match record.action with
                          | Begin i -> true
                          | _ -> false) paired
                 in let begin_indices = List.map (fun x -> let (a,b) = x in b) begins
                                          in begins

let time_asleep_accumulator acc value = 
  match value.action with
    
let calc_guard_stats guard_records =
  let foo = 1 in
  let to
  { total = foo;
    min = foo;
    }

let get_guard_stats records = 
  let ids = List.sort_uniq compare (List.map (fun x -> x.id) records) in
  let filtered_records = (List.map (fun x -> (x, List.filter (fun y -> y.id = x) records)) ids) in
  let stats = List.map (fun x -> (fst x, calc_guard_stats (snd x))) filtered_records in
  stats
  

(*
Examples of strings
[1518-09-09 00:57] wakes up
[1518-04-22 00:30] falls asleep
[1518-08-30 00:00] Guard #1279 begins shift
 *)
