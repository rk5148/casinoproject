type object_phrase = string list

type command =
  | Quit
  | Family
  | Bank
  | Play of object_phrase

exception Empty
exception Malformed

let command_without_first (command : string list) =
  match command with
  | [] -> []
  | h :: t -> t

let command_to_list (exit : string) =
  exit |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")

let first_element_of_command (command_list : string list) =
  List.nth command_list 0

let is_play_correctly_formed (command_list_without_go : string list) =
  if List.length command_list_without_go <= 0 then false else true

let is_quit_correctly_formed (command_list_without_quit : string list) =
  if List.length command_list_without_quit > 0 then false else true

let is_bank_correctly_formed (command_list_without_quit : string list) =
  if List.length command_list_without_quit > 0 then false else true

let is_family_correctly_formed (command_list_without_quit : string list)
    =
  if List.length command_list_without_quit > 0 then false else true

let is_string_empty (command_list : string list) =
  if List.length command_list = 0 then true else false

let parse str =
  if is_string_empty (command_to_list str) then raise Empty
  else if first_element_of_command (command_to_list str) = "quit" then
    if
      is_quit_correctly_formed
        (command_without_first (command_to_list str))
    then Quit
    else raise Malformed
  else if first_element_of_command (command_to_list str) = "play" then
    if
      is_play_correctly_formed
        (command_without_first (command_to_list str))
    then Play (command_without_first (command_to_list str))
    else raise Malformed
  else if first_element_of_command (command_to_list str) = "bank" then
    if
      is_bank_correctly_formed
        (command_without_first (command_to_list str))
    then Bank
    else raise Malformed
  else if first_element_of_command (command_to_list str) = "family" then
    if
      is_family_correctly_formed
        (command_without_first (command_to_list str))
    then Family
    else raise Malformed
  else raise Malformed
