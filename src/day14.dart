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

    /// Returns the number of steps needed for this robot to reach `pos`.
    int? steps_until_at(Position pos) {
        // The number of steps needed for the robot to reach `(pos.x, _)`.
        // We will then have `this.x + this.vx * x_steps == pos.x`:
        int x_steps = (rem(pos.x - this.x, WIDTH) * inv(this.vx, WIDTH)) % WIDTH;

        // The total number of steps will be `x_steps + y_steps * WIDTH`,
        // since doing `WIDTH` steps will not move the robot horizontally.
        // This means that we need to figure out by how much the robot moves
        // vertically after `WIDTH` steps:
        int vy = (this.vy * WIDTH) % HEIGHT;
        // ...and where it begins vertically after `x_steps`:
        int py = (this.y + this.vy * x_steps) % HEIGHT;
        // The number of steps needed for `py + vy * y_steps == pos.y` to be true:
        int y_steps = (rem(pos.y - py, HEIGHT) * inv(vy, HEIGHT)) % HEIGHT;

        var steps = x_steps + y_steps * WIDTH;

        // If we're unlucky and get a robot that has a period of `WIDTH` or `HEIGHT`,
        // then this little test will catch it:
        var final_position = this.step(steps);
        if (final_position.x != pos.x || final_position.y != pos.y) {
            return null;
        }
        return steps;
    }

    /// Computes the number of steps until `friend` is next to this robot.
    /// In other words: `friend.step(result) - this.step(result) = neighbor_pos`
    int? steps_until_adjacent(Robot friend, Position neighbor_pos) {
        var witness = Robot(
            rem(friend.x - this.x, WIDTH),
            rem(friend.y - this.y, HEIGHT),
            rem(friend.vx - this.vx, WIDTH),
            rem(friend.vy - this.vy, HEIGHT)
        );

        // The problem is equivalent to looking at an imaginary robot moving by their difference
        // in velocity, and seeing when it reached `neighbor_pos`.
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

    // Part 1, runs in `O(n^2)` assuming that the number of robots is
    // proportional to `width * height = n^2`:
    print(quadrants.fold<int>(1, (prod, x) => (prod * x)));

    var best_score = 0;
    var best_step = 0;

    // Part 2 is unlike most solutions that you might find.
    // Instead of simulating the movement of each 500 robots over
    // 10000 steps, I instead compute the time it takes for random
    // pairs of robots to be next to one another.
    // With the assumption that the number of robots is proportional
    // to `width * height`, this brute-force method runs in `O(n^4)`.
    //
    // Since it's more likely that all robots are next to one another
    // on the easter egg frame, I can compute the distribution of
    // frames at which robots are adjacent to one another and find the
    // one where they meet up the most often.
    //
    // This lets me run in `O(n^2 * log(n) * s(ε))`, which could be sped up to
    // `O(n^2 * s(ε))` if I memoized the finite field inversion function.
    // The function `s(ε)` corresponds to the `steps` variable below, and is a
    // bit tricky to compute formally, but I doubt it will be anything above `log^2(n)/ε`.
    // I found that the value below performed well enough
    // and experimentally yielded `ε < 3%` (p=0.95).
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
