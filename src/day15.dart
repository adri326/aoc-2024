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

    Grid(this.width, this.height) :
        this.cells = List.generate(width * height, (_) => Cell.empty);

    void set(int x, int y, Cell state) {
        this.cells[y * this.width + x] = state;
    }

    Cell? get(int x, int y) {
        if (x < 0 || y < 0 || x >= this.width || y >= this.height) return null;
        return this.cells[y * this.width + x];
    }

    bool can_move((int, int) robot, Move dir) {
        int dist = 1;
        var delta = move_to_dir(dir);

        while (true) {
            int x = robot.$1 + delta.$1 * dist;
            int y = robot.$2 + delta.$2 * dist;

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

    void move((int, int) robot, Move dir) {
        int dist = 1;
        var delta = move_to_dir(dir);

        while (true) {
            int x = robot.$1 + delta.$1 * dist;
            int y = robot.$2 + delta.$2 * dist;

            switch (this.get(x, y)) {
                case null:
                    return;
                case Cell.box:
                    if (dist == 1) {
                        this.set(x, y, Cell.empty);
                    }
                    dist += 1;
                    continue;
                case Cell.wall:
                    return;
                case Cell.empty:
                    if (dist > 1) {
                        this.set(x, y, Cell.box);
                    }
                    return;
            }
        }
    }

    void print((int, int) robot) {
        for (int y = 0; y < this.height; y++) {
            for (int x = 0; x < this.width; x++) {
                if (x == robot.$1 && y == robot.$2) {
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

void main() async {
    var input = await File("./input/day15.txt").readAsString();

    bool is_maze = true;
    List<Move> moves = [];
    var maze = Grid(input.split("\n")[0].length, input.split("\n").indexWhere((line) => line == ""));
    var robot = (0, 0);

    for (var (y, line) in input.split("\n").indexed) {
        if (line == "") {
            is_maze = false;
            continue;
        }

        if (is_maze) {
            for (var (x, char) in line.split("").indexed) {
            switch (char) {
                case '@':
                robot = (x, y);
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

    for (var move in moves) {
        if (maze.can_move(robot, move)) {
            var dir = move_to_dir(move);
            maze.move(robot, move);
            robot = (robot.$1 + dir.$1, robot.$2 + dir.$2);
        }
    }

    maze.print(robot);
    print(maze.score());
}
