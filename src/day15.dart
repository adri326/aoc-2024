import "dart:io";

enum Move {
    top,
    right,
    left,
    bottom,
}

(int, int) move_to_dir(Move mv) {
    switch (mv) {
        case Move.top:
            return (0, -1);
        case Move.right:
            return (1, 0);
        case Move.left:
            return (-1, 0);
        case Move.bottom:
            return (0, 1);
    }
}

enum Cell {
    empty,
    wall,
    box,
}

class Grid {
    List<Cell> cells;
    int width;
    int height;
    int robot_x = 0;
    int robot_y = 0;

    Grid(this.width, this.height) :
        this.cells = List.generate(width * height, (_) => Cell.empty);

    void set(int x, int y, Cell state) {
        this.cells[y * this.width + x] = state;
    }

    Cell? get(int x, int y) {
        if (x < 0 || y < 0 || x >= this.width || y >= this.height) return null;
        return this.cells[y * this.width + x];
    }

    bool can_move(Move dir) {
        int dist = 1;
        var delta = move_to_dir(dir);

        while (true) {
            int x = this.robot_x + delta.$1 * dist;
            int y = this.robot_y + delta.$2 * dist;

            switch (this.get(x, y)) {
                case null:
                    return false;
                case Cell.box:
                    dist += 1;
                    continue;
                case Cell.wall:
                    return false;
                case Cell.empty:
                    return true;
            }
        }
    }

    void move(Move dir) {
        int dist = 1;
        var delta = move_to_dir(dir);

        while (true) {
            int x = this.robot_x + delta.$1 * dist;
            int y = this.robot_y + delta.$2 * dist;

            switch (this.get(x, y)) {
                case null:
                    throw "Unreachable";
                case Cell.box:
                    if (dist == 1) {
                        this.set(x, y, Cell.empty);
                    }
                    dist += 1;
                    continue;
                case Cell.wall:
                    throw "Unreachable";
                case Cell.empty:
                    if (dist > 1) {
                        this.set(x, y, Cell.box);
                    }
                    this.robot_x += delta.$1;
                    this.robot_y += delta.$2;
                    return;
            }
        }
    }

    void print() {
        for (int y = 0; y < this.height; y++) {
            for (int x = 0; x < this.width; x++) {
                if (x == this.robot_x && y == this.robot_y) {
                    stdout.write("@");
                    continue;
                }

                switch (this.cells[y * this.width + x]) {
                    case Cell.wall:
                        stdout.write("#");
                    case Cell.box:
                        stdout.write("O");
                    case Cell.empty:
                        stdout.write(".");
                }
            }
            stdout.write("\n");
        }
    }

    int score() {
        int res = 0;

        for (int y = 0; y < this.height; y++) {
            for (int x = 0; x < this.width; x++) {
                if (this.cells[y * this.width + x] == Cell.box) {
                    res += y * 100 + x;
                }
            }
        }

        return res;
    }
}

enum WideCell {
    wall,
    box_left,
    box_right,
    empty,
}

class WideGrid {
    List<WideCell> cells;
    int width;
    int height;

    int robot_x;
    int robot_y;

    WideGrid(Grid grid) :
        width = grid.width * 2,
        height = grid.height,
        robot_x = grid.robot_x * 2,
        robot_y = grid.robot_y,
        cells = grid.cells.expand((cell) {
            switch (cell) {
                case Cell.wall:
                    return [WideCell.wall, WideCell.wall];
                case Cell.box:
                    return [WideCell.box_left, WideCell.box_right];
                case Cell.empty:
                    return [WideCell.empty, WideCell.empty];
            }
        }).toList();

    WideCell? get(int x, int y) {
        if (x < 0 || y < 0 || x >= this.width || y >= this.height) return null;
        return this.cells[y * this.width + x];
    }


    void set(int x, int y, WideCell state) {
        this.cells[y * this.width + x] = state;
    }

    bool can_move_sub(Move dir, (int, int) pos) {
        var delta = move_to_dir(dir);
        int x = pos.$1 + delta.$1;
        int y = pos.$2 + delta.$2;

        switch (this.get(x, y)) {
            case null:
            case WideCell.wall:
                return false;
            case WideCell.empty:
                return true;
            case WideCell.box_left:
                if (delta.$2 != 0) {
                    return this.can_move_sub(dir, (x, y)) && this.can_move_sub(dir, (x + 1, y));
                } else {
                    return this.can_move_sub(dir, (x + 1, y));
                }
            case WideCell.box_right:
                if (delta.$2 != 0) {
                    return this.can_move_sub(dir, (x, y)) && this.can_move_sub(dir, (x - 1, y));
                } else {
                    return this.can_move_sub(dir, (x - 1, y));
                }
        }
    }

    bool can_move(Move dir) {
        return this.can_move_sub(dir, (this.robot_x, this.robot_y));
    }

    void move_sub(Move dir, (int, int) pos) {

        var delta = move_to_dir(dir);
        int x = pos.$1 + delta.$1;
        int y = pos.$2 + delta.$2;

        switch (this.get(x, y)) {
            case null:
            case WideCell.wall:
                throw "Unreachable";
            case WideCell.empty:
                return;
            case WideCell.box_left:
                if (delta.$2 != 0) {
                    this.move_sub(dir, (x, y));
                    if (this.get(x, y + delta.$2) != WideCell.box_left) {
                        this.move_sub(dir, (x + 1, y));
                    }

                    this.set(x, y, WideCell.empty);
                    this.set(x + 1, y, WideCell.empty);
                    this.set(x, y + delta.$2, WideCell.box_left);
                    this.set(x + 1, y + delta.$2, WideCell.box_right);
                } else {
                    this.move_sub(dir, (x + 1, y));

                    this.set(x, y, WideCell.empty);
                    this.set(x + 1, y, WideCell.box_left);
                    this.set(x + 2, y, WideCell.box_right);
                }
            case WideCell.box_right:
                if (delta.$2 != 0) {
                    this.move_sub(dir, (x, y));
                    if (this.get(x, y + delta.$2) != WideCell.box_right) {
                        this.move_sub(dir, (x - 1, y));
                    }

                    this.set(x, y, WideCell.empty);
                    this.set(x - 1, y, WideCell.empty);
                    this.set(x, y + delta.$2, WideCell.box_right);
                    this.set(x - 1, y + delta.$2, WideCell.box_left);
                } else {
                    this.move_sub(dir, (x - 1, y));

                    this.set(x, y, WideCell.empty);
                    this.set(x - 1, y, WideCell.box_right);
                    this.set(x - 2, y, WideCell.box_left);
                }
        }
    }

    void move(Move dir) {
        var delta = move_to_dir(dir);
        this.move_sub(dir, (this.robot_x, this.robot_y));
        this.robot_x += delta.$1;
        this.robot_y += delta.$2;
    }


    int score() {
        int res = 0;

        for (int y = 0; y < this.height; y++) {
            for (int x = 0; x < this.width; x++) {
                if (this.cells[y * this.width + x] == WideCell.box_left) {
                    res += y * 100 + x;
                }
            }
        }

        return res;
    }

    void print() {
        for (int y = 0; y < this.height; y++) {
            for (int x = 0; x < this.width; x++) {
                if (x == this.robot_x && y == this.robot_y) {
                    stdout.write("@");
                    continue;
                }

                switch (this.cells[y * this.width + x]) {
                    case WideCell.wall:
                        stdout.write("#");
                    case WideCell.box_left:
                        stdout.write("[");
                    case WideCell.box_right:
                        stdout.write("]");
                    case WideCell.empty:
                        stdout.write(".");
                }
            }
            stdout.write("\n");
        }
    }
}

void main() async {
    var input = await File("./input/day15.txt").readAsString();

    bool is_maze = true;
    List<Move> moves = [];
    var maze = Grid(input.split("\n")[0].length, input.split("\n").indexWhere((line) => line == ""));

    for (var (y, line) in input.split("\n").indexed) {
        if (line == "") {
            is_maze = false;
            continue;
        }

        if (is_maze) {
            for (var (x, char) in line.split("").indexed) {
            switch (char) {
                case '@':
                    maze.robot_x = x;
                    maze.robot_y = y;
                case '#':
                maze.set(x, y, Cell.wall);
                case 'O':
                maze.set(x, y, Cell.box);
            }
            }
        } else {
            for (var char in line.split("")) {
            switch (char) {
                case '^':
                moves.add(Move.top);
                case '<':
                moves.add(Move.left);
                case '>':
                moves.add(Move.right);
                case 'v':
                moves.add(Move.bottom);
            }
            }
        }
    }

    var wide_maze = WideGrid(maze);

    for (var move in moves) {
        if (maze.can_move(move)) {
            maze.move(move);
        }

        if (wide_maze.can_move(move)) {
            wide_maze.move(move);
        }
    }

    maze.print();
    print(maze.score());

    wide_maze.print();
    print(wide_maze.score());
}
