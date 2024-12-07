type unsolved = {
  result: int;
  operands: int list;
}

type operator =
  | Add
  | Mul
  | Concat

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

let next_power (base: int) (value: int): int =
  let rec find_next_power (guess: int): int =
    if guess > value then
      guess
    else
      find_next_power (guess * base)
  in
  find_next_power 1

let option_or opt or_else =
  match opt with
  | Some res -> Some res
  | None -> or_else ()

let rec find_operators_sub (operands: int list) (remaining_result: int) (allow_concat: bool): operator list option =
  let try_mul (head: int) (rest: int list) (remaining_result: int): operator list option =
    if remaining_result mod head == 0 then
      match find_operators_sub rest (remaining_result / head) allow_concat with
      | Some ops -> Some (Mul :: ops)
      | None -> None
    else
      None
  in
  let try_add (head: int) (rest: int list) (remaining_result: int): operator list option =
    if remaining_result - head >= 0 then
      match find_operators_sub rest (remaining_result - head) allow_concat with
      | Some ops -> Some (Add :: ops)
      | None -> None
    else
      None
  in
  let try_concat (head: int) (rest: int list) (remaining_result: int): operator list option =
    let ten_power = next_power 10 head in
    if allow_concat && remaining_result mod ten_power == head then
      match find_operators_sub rest ((remaining_result - head) / ten_power) allow_concat with
      | Some ops -> Some (Concat :: ops)
      | None -> None
    else
      None
  in

  match operands with
  | [] -> if remaining_result == 0 then Some [] else None
  | [head] -> if remaining_result == head then Some [] else None
  | head :: rest ->
    option_or (try_mul head rest remaining_result)
      (fun () ->
        option_or (try_add head rest remaining_result)
        (fun () ->
          try_concat head rest remaining_result
        )
      )

let find_operators (line: unsolved) (allow_concat: bool): operator list option =
  find_operators_sub (List.rev line.operands) line.result allow_concat

let _print_operator (op: operator): unit = match op with
  | Add -> print_string "+"
  | Mul -> print_string "*"
  | Concat -> print_string "|"

let day_file = "./input/day7.txt"
let () =
  let input = read_input (open_in day_file) in
  let input = List.map parse_line (List.filter (fun x -> String.length x > 0) input) in
  (* List.iter (fun line ->
    match find_operators line true with
    | Some ops -> List.iter (fun op -> _print_operator op) ops; print_newline ()
    | None -> print_string "!"; print_newline ()
  ) input; *)
  print_int (List.fold_left (fun sum line ->
    if Option.is_some (find_operators line false) then
      sum + line.result
    else
      sum
  ) 0 input);
  print_newline ();
  print_int (List.fold_left (fun sum line ->
    if Option.is_some (find_operators line true) then
      sum + line.result
    else
      sum
  ) 0 input);
  print_newline ()
