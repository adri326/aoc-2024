type unsolved = {
  result: int;
  operands: int list;
}

type operator =
  | Add
  (* | Sub *)
  | Mul
  (* | Div *)

let read_input chan: string list =
  let res = ref [] in
  try
    while true do
      let line = input_line chan in
      res := List.append !res [line]
    done
  with
    End_of_file ->
      close_in_noerr chan;
      !res

let parse_line (line: string): unsolved =
  match String.split_on_char ':' line with
  | [result; operands] ->
    {
      result = int_of_string result;
      operands = (String.split_on_char ' ' operands)
        |> List.filter (fun x -> String.length x > 0)
        |> List.map int_of_string
    }
  | _ ->
    raise (Failure "Invalid line")



let rec find_operators_sub (operands: int list) (remaining_result: int): operator list option =
  let try_mul (head: int) (rest: int list) (remaining_result: int): operator list option =
    if remaining_result mod head == 0 then
      match find_operators_sub rest (remaining_result / head) with
        | Some ops -> Some (Mul :: ops)
        | None -> None
    else
      None
  in
  let try_add (head: int) (rest: int list) (remaining_result: int): operator list option =
    if remaining_result - head >= 0 then
      match find_operators_sub rest (remaining_result - head) with
        | Some ops -> Some (Add :: ops)
        | None -> None
    else
      None
  in

  match operands with
  | [] -> if remaining_result == 0 then Some [] else None
  | [head] -> if remaining_result == head then Some [] else None
  | head :: rest ->
    match try_mul head rest remaining_result with
    | Some res -> Some res
    | None ->
      try_add head rest remaining_result

let find_operators (line: unsolved): operator list option =
  find_operators_sub (List.rev line.operands) line.result

let _print_operator (op: operator): unit = match op with
  | Add -> print_string "+"
  | Mul -> print_string "*"


let day_file = "./input/day7.txt"
let () =
  let input = read_input (open_in day_file) in
  let input = List.map parse_line (List.filter (fun x -> String.length x > 0) input) in
  (* List.iter (fun line ->
    match find_operators line with
    | Some ops -> List.iter (fun op -> _print_operator op) ops; print_newline ()
    | None -> print_string "Nope"; print_newline ()
  ) input; *)
  print_int (List.fold_left (fun sum line ->
    if Option.is_some (find_operators line) then
      sum + line.result
    else
      sum
  ) 0 input);
  print_newline ()
