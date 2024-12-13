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

Problem = {}

function Problem:new(a, b, target)
    local res = {a = a, b = b, target = target}
    setmetatable(res, self)
    self.__index = self
    return res
end

function parse_problems(raw_input)
    local problems = {}
    local current_problem = Problem:new({}, {}, {})
    for line in raw_input:gmatch("[^\n]*") do
        if line == "" then
            problems[#problems + 1] = current_problem
            current_problem = Problem:new({}, {}, {})
        else
            local prefix = line:sub(1, line:find(":") - 1)
            local x = 0
            local y = 0
            for axis, value in line:gmatch("([XY])[+=](%d+)") do
                if axis == "X" then
                    x = tonumber(value)
                else
                    y = tonumber(value)
                end
            end

            if prefix == "Button A" then
                current_problem.a = {x = x, y = y}
            elseif prefix == "Button B" then
                current_problem.b = {x = x, y = y}
            elseif prefix == "Prize" then
                current_problem.target = {x = x, y = y}
            end
        end
    end

    if #current_problem.a ~= 0 then
        problems[#problems + 1] = current_problem
    end

    return problems
end

function Problem:solve_part1()
    local best_solution = nil

    -- Notice how there is only one loop:
    for a_amount = 0, 100 do
        local x_remaining = self.target.x - self.a.x * a_amount
        if x_remaining % self.b.x == 0 then
            local b_amount = x_remaining // self.b.x

            -- How much we need to travel on the Y axis to reach the prize after
            -- having pressed the A button `a_amount` times.
            local y_remaining = self.target.y - self.a.y * a_amount

            if y_remaining == self.b.y * b_amount then
                local cost = a_amount * 3 + b_amount
                if best_solution == nil or best_solution > cost then
                    best_solution = cost
                end
            end
        end
    end

    return best_solution
end

local problems = parse_problems(read_all("./input/day13.txt"))

local part1 = 0
for _, problem in pairs(problems) do
    local cost = problem:solve_part1()
    if cost ~= nil then
        part1 = part1 + cost
    end
end
print(part1)
