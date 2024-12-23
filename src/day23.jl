using Graphs


# function parse_computer(str :: AbstractString) :: Int16
#     26 * (str[1] - 'a') + str[2] - 'a'
# end

function read_input(path = "./input/day23-example.txt")
    open(path, "r") do f
        graph = SimpleGraph{Int16}(0)
        name_map = Dict{String, Int16}()
        while !eof(f)
            line = readline(f)
            if line == ""
                continue
            end
            lhs, rhs = split(line, "-")
            if !(lhs in keys(name_map))
                add_vertex!(graph)
                merge!(name_map, Dict{String, Int16}(lhs => nv(graph)))
            end
            if !(rhs in keys(name_map))
                add_vertex!(graph)
                merge!(name_map, Dict{String, Int16}(rhs => nv(graph)))
            end
            add_edge!(graph, name_map[lhs], name_map[rhs])
        end
        (graph, name_map)
    end
end

# Janky solution, but oh well
function part1(path = "./input/day23-example.txt")
    graph, name_map = read_input(path)
    all_triangles = 0
    double_triangles = 0
    triple_triangles = 0

    t_nodes = values(filter((v) -> startswith(v[1], "t"), name_map))
    t_nodes_set = Set(t_nodes)

    for node in t_nodes
        all_triangles += triangles(graph, node)

        for other in t_nodes
            if other > node && has_edge(graph, node, other)
                shared = ∩(([node, other] .|> x -> Set(neighbors(graph, x)))...)

                double_triangles += length(shared)

                triple_triangles += length(∩(shared, t_nodes_set))
            end
        end
    end

    all_triangles - double_triangles + triple_triangles ÷ 3
end

function get_name(name_map, id)
    for (name, value) in name_map
        if value == id
            return name
        end
    end
end

function part2(path = "./input/day23-example.txt")
    graph, name_map = read_input(path)

    max_clique = argmax(maximal_cliques(graph)) do clique
        length(clique)
    end .|> x -> get_name(name_map, x)

    join(sort(max_clique), ",")
end
