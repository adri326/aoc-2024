type direction =
  Top
  | Right
  | Bottom
  | Left

type cell =
  Empty
  | Wall
  | Traversed of direction

type grid = {
  cells: cell array;
  width: int;
  height: int;
}

type guard = {
  x: int;
  y: int;
  dir: direction
}

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

let parse_input (raw: string list): grid * guard =
  let width = String.length (List.hd raw) in
  let height = List.length raw in
  let cells = (Array.make (width * height) Empty) in
  List.iteri (fun y ->
    String.iteri (fun x c ->
      if c == '#' then
        cells.(y * width + x) <- Wall
    )
  ) raw;
  let y = Option.get (List.find_index (fun row -> String.contains row '^') raw) in
  let x = String.index (List.nth raw y) '^' in

  (
    {
      width = width;
      height = height;
      cells = cells
    },
    {
      x = x;
      y = y;
      dir = Top;
    }
  )

let get_cell (g: grid) (x: int) (y: int): cell option =
  if x < 0 || y < 0 || x >= g.width || y >= g.height then
    None
  else
    Some g.cells.(y * g.width + x)

let set_cell (g: grid) (x: int) (y: int) (value: cell): unit =
  if x < 0 || y < 0 || x >= g.width || y >= g.height then
    ()
  else
    g.cells.(y * g.width + x) <- value

let copy_grid (g: grid): grid =
  {
    width = g.width;
    height = g.height;
    cells = Array.copy g.cells
  }

let cell_to_string (c: cell): string = match c with
  | Empty -> "."
  | Wall -> "#"
  | Traversed _ -> "X"

let _print_grid (g: grid): unit =
  for y = 0 to g.height - 1 do
    for x = 0 to g.width - 1 do
      print_string (cell_to_string (Option.get (get_cell g x y)))
    done;
    print_string "\n"
  done

let turn_right (dir: direction): direction = match dir with
  | Top -> Right
  | Right -> Bottom
  | Bottom -> Left
  | Left -> Top

let advance (dir: direction) (x: int) (y: int): int * int = match dir with
  | Top -> (x, y - 1)
  | Right -> (x + 1, y)
  | Bottom -> (x, y + 1)
  | Left -> (x - 1, y)

let guard_step (g: grid) (guard: guard): guard =
  if Option.is_none (get_cell g guard.x guard.y) then
    guard
  else
    let (x, y) = advance guard.dir guard.x guard.y in
    let (x, y, dir) = match get_cell g x y with
      | Some Wall -> (guard.x, guard.y, turn_right guard.dir)
      | _ -> (x, y, guard.dir)
    in
    (
      match get_cell g guard.x guard.y with
      | Some Empty -> set_cell g guard.x guard.y (Traversed guard.dir)
      | _ -> ()
    );
    {
      x = x;
      y = y;
      dir = dir;
    }

let count_traversed (g: grid): int =
  Array.fold_left (fun sum x ->
    match x with
    | Traversed _ -> sum + 1
    | _ -> sum
  ) 0 g.cells

let is_done grid current =
  match get_cell grid current.x current.y with
  | None -> true
  | Some Empty -> false
  | Some Wall -> false (* Unreachable *)
  | Some Traversed dir -> current.dir == dir

let simulate (input_grid: grid) (guard: guard): grid * bool =
  let current_guard = ref guard in
  let grid = copy_grid input_grid in
    while not (is_done grid !current_guard) do
      current_guard := guard_step grid !current_guard
    done;
    (grid, Option.is_some (get_cell grid !current_guard.x !current_guard.y))

let is_loop (g: grid) (x: int) (y: int) (guard: guard): bool =
  let modified_grid = copy_grid g in
    set_cell modified_grid x y Wall;
    snd (simulate modified_grid guard)

let part1 (input_grid: grid) (guard: guard): unit =
  print_int (count_traversed (fst (simulate input_grid guard)));
  print_string "\n"

let part2 (input_grid: grid) (guard: guard): unit =
  let is_traversed (cell: cell): bool =
    match cell with
    | Traversed _ -> true
    | _ -> false
  in

  let first_pass = fst (simulate input_grid guard) in

  (* Iterate over the cells in first_pass that have been traversed and see if adding a wall results in a loop. *)
  let loops = Array.mapi (fun i cell ->
    let x = i mod input_grid.width in
    let y = i / input_grid.width in
    if not (x == guard.x && y == guard.y) && is_traversed cell then
      is_loop input_grid x y guard
    else
      false
  ) first_pass.cells in

  (* Sum up all cells resulting in a loop. *)
  print_int (Array.fold_left (fun sum x ->
    if x then
      sum + 1
    else
      sum
  ) 0 loops);
  print_string "\n"

let day_file = "./input/day6.txt"
let () =
  let input = read_input (open_in day_file) in
  let (input_grid, guard) = parse_input input in
    part1 input_grid guard;
    part2 input_grid guard
