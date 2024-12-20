using Memoization

function read_input(path = "./input/day19.txt")
    open(path, "r") do f
        patterns = Vector{String}()
        towels = split(readline(f), ", ")
        while !eof(f)
            s = readline(f)
            if s != ""
                push!(patterns, s)
            end
        end
        (towels, patterns)
    end
end

# DFS search
@memoize function can_generate(towels, pattern)
    if pattern == ""
        return true
    end
    for towel in towels
        if startswith(pattern, towel)
            if can_generate(towels, pattern[(length(towel) + 1):length(pattern)])
                return true
            end
        end
    end
    return false
end

function num_ways(towel_map, pattern)
    counts = fill(Int64(0), length(pattern) + 1)
    index = length(pattern) + 1
    counts[length(pattern) + 1] = 1

    # Look, no recursion!
    while index > 1
        index = index - 1
        count = 0
        for towel in get(towel_map, pattern[index], [])
            if startswith(pattern[index:length(pattern)], towel)
                count += counts[index + length(towel)]
            end
        end
        counts[index] = count
    end

    counts[1]
end

function build_towel_map(towels)
    res = Dict{Char, Vector{String}}()
    for towel in towels
        prev = copy(get(res, towel[1], Vector{String}()))
        push!(prev, towel)
        merge!(res, Dict{Char, Vector{String}}(towel[1] => prev))
    end
    res
end

function part1(path = "./input/day19.txt")
    towels, patterns = read_input(path)

    count(pattern -> can_generate(towels, pattern), patterns)
end

function part2(path = "./input/day19.txt")
    towels, patterns = read_input(path)
    towel_map = build_towel_map(towels)

    sum(pattern -> num_ways(towel_map, pattern), patterns)
end
