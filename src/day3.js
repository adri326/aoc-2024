// Sorry, I'm not solving this problem in Ada if the language doesn't have regexes.

import fs from "node:fs";

const input = fs.readFileSync("./input/day3.txt", "utf8");

function part1() {
    let sum = 0;
    const regex = /mul\((\w+),(\w+)\)/g;
    for (let match; match = regex.exec(input);) {
        sum += +match[1] * +match[2];
    }
    console.log(sum);
}

function part2() {
    let sum = 0;

    const regex = /(?<op>mul)\((\w+),(\w+)\)|(?<op>do)\(\)|(?<op>don't)\(\)/g;

    let enabled = true;
    for (let match; match = regex.exec(input);) {
        if (match.groups.op === "do") enabled = true;
        else if (match.groups.op === "don't") enabled = false;
        else if (match.groups.op === "mul" && enabled) {
            sum += +match[2] * +match[3];
        }
    }

    console.log(sum);
}

part1();
part2();
