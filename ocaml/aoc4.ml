open Batteries

(* from https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

type guard_action = Begin of string
                  | Sleep 
                  | Wake

type guard_record = 
  { time : Date.t ;
    action: guard_action;
  }

let parse_record string : guard_record =
  { time = Date.now;
    action: Begin ("10")
  }
(*
Examples of strings
[1518-09-09 00:57] wakes up
[1518-04-22 00:30] falls asleep
[1518-08-30 00:00] Guard #1279 begins shift
 *)


let main () =
  (1--999) (* the enum that counts from 1 to 999 *)
  |> Enum.filter (fun i -> i mod 3 = 0 || i mod 5 = 0)
  |> Enum.reduce (+) (* add all remaining values together *)
  |> Int.print stdout
