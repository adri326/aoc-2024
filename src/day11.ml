let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g

let curry f x y = f (x, y)

let count_stones: int -> int -> int =
  let count self (pair: int * int) =
    let self = curry self in
    let stone = fst pair in
    let gas = snd pair in
    if gas = 0 then
      1
    else
      if stone = 0 then
        self 1 (gas - 1)
      else
        let stone_str = string_of_int stone in
        let str_len = String.length stone_str in
        if str_len mod 2 == 0 then
          self (int_of_string (String.sub stone_str 0 (str_len / 2))) (gas - 1)
          + self (int_of_string (String.sub stone_str (str_len / 2) (str_len / 2))) (gas - 1)
        else
          self (stone * 2024) (gas - 1)
  in
  curry (memo_rec count)

let () =
  let input = String.split_on_char ' ' (input_line (open_in "./input/day11.txt")) in
  let part1 = List.fold_left (fun acc stone ->
    acc + (count_stones (int_of_string stone) 25)
  ) 0 input in
  print_int part1; print_newline ();

  let part2 = List.fold_left (fun acc stone ->
    acc + (count_stones (int_of_string stone) 75)
  ) 0 input in
  print_int part2; print_newline ()
