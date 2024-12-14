import "dart:io";
import "dart:math";

// For the example, use 7 and 11
const WIDTH = 101;
const HEIGHT = 103;
const List<Position> NEIGHBORS = [
    Position(-1, 0),
    Position(1, 0),
    Position(0, -1),
    Position(0, 1),
];

class Position {
    final int x;
    final int y;

    const Position(this.x, this.y);

    Position operator+(Position p) {
        return Position((this.x + p.x) % WIDTH, (this.y + p.y) % HEIGHT);
    }

    int quadrant() {
        if (this.x == (WIDTH - 1) / 2) return -1;
        if (this.y == (HEIGHT - 1) / 2) return -1;

        int res = 0;
        if (this.y > (HEIGHT - 1) / 2) res += 2;
        if (this.x > (WIDTH - 1) / 2) res += 1;
        return res;
    }
}

int rem(int x, int modulo) {
    if (x < 0) return (x + modulo) % modulo;
    else return x % modulo;
}

// Runs in O(log(exponent))
int pow(int base, int exponent, int modulo) {
    int current = base;
    int res = 1;
    while (exponent > 0) {
        if ((exponent & 1) == 1) {
            res = (res * current) % modulo;
        }
        current = (current * current) % modulo;
        exponent >>= 1;
    }
    return res;
}

int inv(int x, int modulo) {
    return pow(x, modulo - 2, modulo);
}

class Robot {
    int x;
    int y;
    int vx;
    int vy;

    Robot(this.x, this.y, this.vx, this.vy) {}

    Position step(int times) {
        return Position((this.x + this.vx * times) % WIDTH, (this.y + this.vy * times) % HEIGHT);
    }

    void step_single() {
        this.x = (this.x + this.vx) % WIDTH;
        this.y = (this.y + this.vy) % HEIGHT;
    }

    int? steps_until_at(Position pos) {
        int x_steps = (rem(pos.x - this.x, WIDTH) * inv(this.vx, WIDTH)) % WIDTH;
        int vy = (this.vy * WIDTH) % HEIGHT;
        int py = (this.y + this.vy * x_steps) % HEIGHT;
        int y_steps = (rem(pos.y - py, HEIGHT) * inv(vy, HEIGHT)) % HEIGHT;
        var steps = x_steps + y_steps * WIDTH;

        var final_position = this.step(steps);
        if (final_position.x != pos.x || final_position.y != pos.y) {
            return null;
        }
        return steps;
    }

    int? steps_until_adjacent(Robot friend, Position neighbor_pos) {
        var witness = Robot(
            rem(friend.x - this.x, WIDTH),
            rem(friend.y - this.y, HEIGHT),
            rem(friend.vx - this.vx, WIDTH),
            rem(friend.vy - this.vy, HEIGHT)
        );

        return witness.steps_until_at(Position(rem(neighbor_pos.x, WIDTH), rem(neighbor_pos.y, HEIGHT)));
    }
}



void main() async {
    var input = await File("./input/day14.txt").readAsString();
    var parse_line = RegExp(r"^p=(\d+),(\d+) v=(-?\d+),(-?\d+)$");
    List<Robot> robots = [];

    for (var line in input.split("\n")) {
        if (line.length == 0) continue;
        var match = parse_line.firstMatch(line);
        if (match == null) continue;
        var values = List.generate(4, (i) => int.parse(match.group(i + 1) ?? "0"));

        var robot = Robot(values[0], values[1], values[2], values[3]);
        robots.add(robot);
    }

    var quadrants = [0, 0, 0, 0];
    for (var robot in robots) {
        var end_position = robot.step(100);
        var quadrant = end_position.quadrant();
        if (quadrant == -1) continue;

        quadrants[quadrant] += 1;
    }

    print(quadrants.fold<int>(1, (prod, x) => (prod * x)));

    var best_score = 0;
    var best_step = 0;

    List<int> counts = List.generate(WIDTH * HEIGHT, (_) => 0);

    var steps = 10 * WIDTH * HEIGHT ~/ robots.length;
    var rng = Random();
    for (var (robot_index, robot) in robots.indexed) {
        for (var i = 0; i < steps; i++) {
            var index = rng.nextInt(robots.length);
            if (index == robot_index) continue;
            var neighbor_pos = NEIGHBORS[rng.nextInt(4)];

            var steps = robot.steps_until_adjacent(robots[index], neighbor_pos);
            if (steps == null) continue;
            counts[steps] += 1;
        }
    }

    for (var i = 0; i < WIDTH * HEIGHT; i++) {
        if (counts[i] > best_score) {
            best_step = i;
            best_score = counts[i];
        }
    }

    print(best_step.toString() + " (score=" + best_score.toString() + ")");
    var tree_positions = robots.map((robot) => robot.step(best_step)).toList();
    for (var y = 0; y < HEIGHT; y++) {
        for (var x = 0; x < WIDTH; x++) {
            int count = 0;
            for (var position in tree_positions) {
                if (position.x == x && position.y == y) {
                    count += 1;
                }
            }
            if (count > 0) {
                stdout.write("x");
            } else {
                stdout.write(".");
            }
        }
        stdout.write("\n");
    }
}
