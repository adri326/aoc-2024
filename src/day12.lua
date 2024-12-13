function dump(o)
    if type(o) == 'table' then
        local s = '{ '
        for k,v in pairs(o) do
            if type(k) ~= 'number' then k = '"'..k..'"' end
            s = s .. '['..k..'] = ' .. dump(v) .. ', '
        end
        return s .. '} '
    else
        return tostring(o)
    end
end

function read_all(path)
    local file = assert(io.open(path, "rb"))
    local content = file:read("*all")
    file:close()
    return content
end

function str_to_table(string)
    local res = {}
    for i = 1, #string do
        res[i] = string:sub(i, i)
    end
    return res
end

lines = {}
filled = {}
for line in read_all("./input/day12.txt"):gmatch("[^\n]+") do
    lines[#lines + 1] = str_to_table(line)
    filled[#filled + 1] = {}
    for i = 1, #line do
        filled[#filled][i] = false
    end
end

height = #lines
width = #lines[1]
neighbors = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}}

function fence_cost(x, y)
    open = {{x, y}}
    closed = {}
    current_char = lines[y][x]

    while #open > 0 do
        local current = table.remove(open, 1)
        local index = (current[2] - 1) * width + current[1] - 1

        if closed[index] == nil then
            local fences = 0
            local sides = {}
            for i, neighbor in pairs(neighbors) do
                local x2 = current[1] + neighbor[1]
                local y2 = current[2] + neighbor[2]

                if
                    x2 >= 1
                    and y2 >= 1
                    and x2 <= width
                    and y2 <= height
                    and lines[y2][x2] == current_char
                then
                    if closed[(y2 - 1) * width + (x2 - 1)] == nil then
                        open[#open + 1] = {x2, y2}
                    end
                    sides[i] = true
                else
                    sides[i] = false
                    fences = fences + 1
                end
            end

            local corners = 0
            for i = 1, 4 do
                j = i % 4 + 1
                if sides[i] and sides[j] then
                    local x2 = current[1] + neighbors[i][1] + neighbors[j][1]
                    local y2 = current[2] + neighbors[i][2] + neighbors[j][2]
                    if lines[y2][x2] ~= current_char then
                        corners = corners + 1
                    end
                elseif not sides[i] and not sides[j] then
                    corners = corners + 1
                end
            end

            closed[index] = {fences, corners}
        end
    end

    local perimeter = 0
    local area = 0
    local sides = 0
    for index, values in pairs(closed) do
        local x2 = index % width + 1
        local y2 = index // width + 1
        filled[y2][x2] = true
        area = area + 1
        perimeter = perimeter + values[1]
        sides = sides + values[2]
    end

    return {area * perimeter, area * sides}
end

part1 = 0
part2 = 0
while true do
    found_unfilled = false
    for y = 1, height do
        for x = 1, width do
            if not filled[y][x] then
                local cost = fence_cost(x, y)
                part1 = part1 + cost[1]
                part2 = part2 + cost[2]
                found_unfilled = true
            end
        end
    end
    if not found_unfilled then
        break
    end
end

print(part1)
print(part2)
