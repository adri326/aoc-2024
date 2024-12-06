type cell =
  Empty
  | Wall
  | Traversed

type grid = {
  cells: cell array;
  width: int;
  height: int;
}

type direction =
  Top
  | Right
  | Bottom
  | Left

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

let cell_to_string (c: cell): string = match c with
  | Empty -> "."
  | Wall -> "#"
  | Traversed -> "X"

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
    match get_cell g x y with
    | Some Wall ->
        {
          x = guard.x;
          y = guard.y;
          dir = turn_right guard.dir
        }
    | _ ->
      set_cell g guard.x guard.y Traversed;
      {
        x = x;
        y = y;
        dir = guard.dir
      }

let count_traversed (g: grid): int =
  Array.fold_left (fun sum x ->
    if x == Traversed then
      sum + 1
    else
      sum
  ) 0 g.cells


let day_file = "./input/day6.txt"

let () =
  let input = read_input (open_in day_file) in
  let (input_grid, guard) = parse_input input in
  let guard = ref guard in
    while Option.is_some (get_cell input_grid !guard.x !guard.y) do
      guard := guard_step input_grid !guard
    done;
    print_int (count_traversed input_grid);
    print_string "\n"
