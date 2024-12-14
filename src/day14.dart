import "dart:io";

// For the example, use 7 and 11
const WIDTH = 101;
const HEIGHT = 103;

class Position {
    int x;
    int y;

    Position(this.x, this.y) {}

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

class Robot {
    int x;
    int y;
    int vx;
    int vy;

    Robot(this.x, this.y, this.vx, this.vy) {}

    Position step(int times) {
        return Position((this.x + this.vx * times) % WIDTH, (this.y + this.vy * times) % HEIGHT);
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
}
