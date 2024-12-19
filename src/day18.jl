function read_input(path = "./input/day18.txt")
    res = []
    open(path, "r") do f
        while !eof(f)
            s = readline(f)
            if s != ""
                push!(res, parse.(Int, split(s, ",")))
            end
        end
    end
    return res
end

function generate_grid(pairs, amount = length(pairs))
    width = maximum(first, pairs) + 1
    height = maximum(x -> x[2], pairs) + 1

    grid = falses(width, height)
    for (x, y) in pairs[1:amount]
        grid[y + 1, x + 1] = true
    end
    return grid
end

function a_star(grid; start = (1, 1), finish = size(grid), epsilon = 1.0)
    function h(pos)
        epsilon * (abs(pos[1] - finish[1]) + abs(pos[2] - finish[2]))
    end

    closed = Set()

    function neighbors(pos)
        width, height = size(grid)
        filter([(1, 0, 1.0), (0, 1, 1.0), (0, -1, 1.0), (-1, 0, 1.0)] .|> delta -> delta .+ pos) do (x2, y2)
            if x2 < 1 || y2 < 1 || x2 > width || y2 > height
                return false
            else
                return !grid[pos[2], pos[1]]
            end
        end
    end

    opens = [(start[1], start[2], 0.0)]

    while length(opens) > 0
        current = splice!(opens, argmin(i -> opens[i][3] + h(opens[i]), 1:length(opens)))
        if current[1:2] in closed
            continue
        end
        push!(closed, current[1:2])
        if isequal(current[1:2], finish)
            return current[3]
        end

        push!(opens, neighbors(current)...)
    end

    return -1
end

# Finds the first value in `range` such that `cb(index)` returns true.
# Assumes that `map(cb, range)` is sorted.
#
# The returned index will be such that `cb(index)` is true and `cb(index - 1)` is false.
function bsearch(cb, range)
    while length(range) > 1
        mid = (first(range) + last(range)) ÷ 2

        if cb(mid)
            range = first(range):mid
        else
            range = (mid + 1):last(range)
        end
    end

    return first(range)
end

function lsearch(cb, range)
    for i in range
        if cb(i)
            return i
        end
    end
    return last(range)
end

function part1(path = "./input/day18.txt", n_bytes = 1024)
    a_star(generate_grid(read_input(path), n_bytes))
end

function part2(path = "./input/day18.txt"; epsilon = 2.0)
    input = read_input(path)

    cutoff = bsearch(1:length(input)) do n_bytes
        a_star(generate_grid(input, n_bytes); epsilon = epsilon) == -1
    end

    x, y = input[cutoff]
    return "$x,$y"
end
