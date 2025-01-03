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

function Problem:is_colinear()
    return self.b.y * self.a.x == self.b.x * self.a.y
end

function max(a, b)
    if a > b then
        return a
    else
        return b
    end
end

function gcd(a, b)
    while b ~= 0 do
        local tmp = b
        b = a % b
        a = tmp
    end
    return a
end

-- Finds and return the pair `{x, y}` with the smallest values `x` and `y`
-- such that `a * x + b * y == target`.
function solve_single_axis(a, b, target)
    if a == 0 then
        if target == 0 then
            return {1, 0}
        elseif target % b == 0 then
            return {0, target // b}
        else
            return nil
        end
    elseif b == 0 then
        if target == 0 then
            return {0, 1}
        elseif target % a == 0 then
            return {target // a, 0}
        else
            return nil
        end
    end

    local c = gcd(math.abs(a), math.abs(b))
    if target % c ~= 0 then
        return nil
    end

    if (a >= 0) == (target >= 0) then
        for x = 1, math.max(math.abs(b), math.abs(target)) / c do
            local y = (target - a * x) // b
            if (target - a * x) % b == 0 and y > 0 then
                return {x, y}
            end
        end
    else
        for y = 1, math.max(math.abs(a), math.abs(target)) / c do
            local x = (target - b * y) // a
            if (target - b * y) % a == 0 and x > 0 then
                return {x, y}
            end
        end
    end

    return nil
end

function Problem:solve_part2()
    local det = self.a.x * self.b.y - self.a.y * self.b.x
    local a_steps = (self.target.x * self.b.y - self.target.y * self.b.x) // det
    local b_steps = (self.target.y * self.a.x - self.target.x * self.a.y) // det

    if self.a.x * a_steps + self.b.x * b_steps ~= self.target.x then
        return nil
    end
    if self.a.y * a_steps + self.b.y * b_steps ~= self.target.y then
        return nil
    end

    return a_steps * 3 + b_steps
end

local problems = parse_problems(read_all("./input/day13.txt"))

local part1 = 0
local part2 = 0
for _, problem in pairs(problems) do
    local cost = problem:solve_part1()
    if cost ~= nil then
        part1 = part1 + cost
    end

    local fixed_problem = Problem:new(problem.a, problem.b, {
        x = problem.target.x + 10000000000000,
        y = problem.target.y + 10000000000000
    })
    local fixed_cost = fixed_problem:solve_part2()
    if fixed_cost ~= nil then
        part2 = part2 + fixed_cost
    end
end
print(part1)
print(part2)
