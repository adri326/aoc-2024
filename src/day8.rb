class Antenna
    attr_accessor :x
    attr_accessor :y
    attr_accessor :freq

    def initialize(x, y, freq)
        @x = x
        @y = y
        @freq = freq
    end

    def within_bounds?(width, height)
        return (@x >= 0 and @y >= 0 and @x < width and @y < height)
    end
end


def antinodes(left, right, width, height, multiples)
    if left.freq != right.freq then
        return []
    else
        dx = right.x - left.x
        dy = right.y - left.y
        res = multiples.map do |m|
            Antenna.new(right.x + m * dx, right.y + m * dy, left.freq)
        end

        return res.filter do |antinode|
            antinode.within_bounds?(width, height)
        end
    end
end

def compute_antinodes(antennas, width, height, multiples)
    all_antinodes = []
    antennas.each_with_index do |antenna, i|
        antennas[(i + 1)..].each do |other|
            antinodes(antenna, other, width, height, multiples).each do |antinode|
                if all_antinodes.all? do |prev_antinode|
                    prev_antinode.x != antinode.x or prev_antinode.y != antinode.y
                end then
                    all_antinodes.push(antinode)
                end
            end
        end
    end
    return all_antinodes
end

def print_grid(input_lines, all_antinodes)
    input_lines.each_with_index do |line, y|
        line.split("").each_with_index do |char, x|
            if char == "." then
                if all_antinodes.any? do |antinode|
                    antinode.x == x and antinode.y == y
                end then
                    print "#"
                else
                    print "."
                end
            else
                print char
            end
        end
    end
end

input_lines = File.open("./input/day8.txt").readlines

antennas = []

input_lines.each_with_index do |line, y|
    line.split("").each_with_index do |char, x|
        if char != "." and char != "\n" then
            antennas.push(Antenna.new(x, y, char))
        end
    end
end
width = input_lines[0].length - 1
height = input_lines.length

print "Part 1: ", compute_antinodes(antennas, width, height, [-2, 1]).length, "\n"
print "Part 2: ", compute_antinodes(antennas, width, height, (-width)..(width + 1)).length, "\n"
