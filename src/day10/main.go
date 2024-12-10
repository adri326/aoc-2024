package main

import (
	"fmt"
	"os"
	"strings"
)

func assert_nil(err error) {
	if err != nil {
		panic(err)
	}
}

type position struct {
	x int
	y int
}

type grid struct {
	values [][]int
	width int
	height int
}

func neighborhood(grid grid, x int, y int) []position {
	res := make([]position, 4)
	len := 0
	current_value := grid.values[y][x]
	for dy := -1; dy <= 1; dy++ {
		for dx := -1; dx <= 1; dx++ {
			x2 := x + dx
			y2 := y + dy
			// Not part of the neighborhood:
			if dx == 0 && dy == 0 || dx != 0 && dy != 0 {
				continue
			}
			// Out of bounds:
			if x2 < 0 || y2 < 0 || x2 >= grid.width || y2 >= grid.height {
				continue
			}
			// Not valid path:
			if grid.values[y2][x2] != current_value + 1 {
				continue
			}
			res[len] = position { x2, y2 }
			len += 1
		}
	}
	return res[:len]
};

func bfs(grid grid, sx int, sy int) int {
	queue := make([]position, 0)
	queue = append(queue, position { sx, sy })
	closed := map[position]bool {}

	for len(queue) > 0 {
		current_pos := queue[0]
		queue = queue[1:]

		_, exists := closed[current_pos]
		if exists {
			continue
		}
		closed[current_pos] = true

		queue = append(queue, neighborhood(grid, current_pos.x, current_pos.y)...)
	}

	res := 0
	for pos := range closed {
		if grid.values[pos.y][pos.x] == 9 {
			res += 1
		}
	}

	return res
}

func dfs(grid grid, sx int, sy int) int {
	queue := make([]position, 0)
	queue = append(queue, position { sx, sy })
	res := 0

	for len(queue) > 0 {
		current_pos := queue[len(queue) - 1]
		queue = queue[:len(queue) - 1]

		if grid.values[current_pos.y][current_pos.x] == 9 {
			res += 1
		}

		queue = append(queue, neighborhood(grid, current_pos.x, current_pos.y)...)
	}

	return res
}

func main() {
	input_data, err := os.ReadFile("./input/day10.txt")
	assert_nil(err)

	input_split := strings.Split(string(input_data), "\n")
	heights := make([][]int, len(input_split))
	height := 0
	width := len(input_split[0])
	for y := 0; y < len(input_split); y++ {
		if len(input_split[y]) > 0 {
			height += 1
		}
		heights[y] = make([]int, len(input_split[y]))
		for x := 0; x < len(input_split[y]); x++ {
			heights[y][x] = int(input_split[y][x]) - int('0');
		}
	}
	heights = heights[:height]

	sum_bfs := 0
	sum_dfs := 0
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			if heights[y][x] == 0 {
				sum_bfs += bfs(grid {
					values: heights,
					width: width,
					height: height,
				}, x, y)
				sum_dfs += dfs(grid {
					values: heights,
					width: width,
					height: height,
				}, x, y)
			}
		}
	}

	fmt.Println(sum_bfs)
	fmt.Println(sum_dfs)
}
